{-# LANGUAGE NamedFieldPuns  #-}
{-# LANGUAGE RankNTypes      #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module Labyrinth.Game
  ( Game(..)
  , Phase(..)
  , DGame(..)
  , DTile(..)
  , player
  , tileAt
  , tiles
  , gates
  , rowSpread
  , colSpread
  , phase
  , rotate
  , rotate'
  , move
  , done
  , walk
  , players
  , make
  )
where

import           Control.Monad                  ( guard )
import qualified Data.List                     as List
import           Data.Map.Strict                ( Map )
import qualified Data.Map.Strict               as Map
import           Data.Maybe                     ( fromJust
                                                , fromMaybe
                                                )
import           Labyrinth.Position             ( Position )
import qualified Labyrinth.Random              as Random
import           Labyrinth.Direction            ( Direction(..) )
import           Labyrinth.Game.Description     ( DGame(..)
                                                , DTile(..)
                                                )
import qualified Labyrinth.Game.Description    as GD
import           Labyrinth.Gate                 ( Gate(..) )
import           Labyrinth.Players              ( Color(..)
                                                , Player
                                                , Players
                                                )
import qualified Labyrinth.Players             as P
import           Labyrinth.Tile                 ( Tile(..) )
import qualified Labyrinth.Tile                as T
import           Lens.Micro                     ( (&)
                                                , (.~)
                                                , (^.)
                                                , _1
                                                , _2
                                                )
import           Lens.Micro.TH                  ( makeLenses )
import           Lens.Micro.Type                ( Lens' )

data Phase = Plan | Walk | End deriving (Show, Eq)

data Game = Game
    { _tileAt  :: Position
    , _tiles   :: Map Position Tile
    , _gates   :: Map Position Gate
    , _phase   :: Phase
    , _rowMin  :: Int
    , _rowMax  :: Int
    , _colMin  :: Int
    , _colMax  :: Int
    , _player  :: Player
    , _players :: Players
    } deriving (Show, Eq)
makeLenses ''Game

make :: DGame -> IO Game
make gd = do
  let startPosition = (0, 2)

  player' <- firstPlayer (gd ^. GD.gPlayers)
  tiles'  <- GD.mkTiles gd

  return Game
    { _tileAt  = startPosition
    , _player  = player'
    , _players = gd ^. GD.gPlayers
    , _gates   = Map.fromList $ gd ^. GD.gGates
    , _tiles   = Map.fromList tiles'
    , _phase   = Plan
    , _rowMin  = gd ^. GD.gRowMin
    , _rowMax  = gd ^. GD.gRowMax
    , _colMin  = gd ^. GD.gColMin
    , _colMax  = gd ^. GD.gColMax
    }

--------------------------------------------------------------------------------
-- state transitions
--------------------------------------------------------------------------------

done :: Game -> Game
done g = case g ^. phase of
  Plan -> fromMaybe g $ do
    Gate _ isOpen <- Map.lookup (g ^. tileAt) (g ^. gates)
    guard isOpen
    return
      $ ( nextPhase
        . teleport
        . toggleGates
        . updateCurrentTilePosition
        . slideTile
        )
          g
  _ -> (nextPhase . nextPlayer) g

slideTile :: Game -> Game
slideTile g = g & tiles .~ Map.mapKeys slide (g ^. tiles)
 where
  (cr, cc) = g ^. tileAt
  slide pos@(r, c) = fromMaybe pos $ do
    edge' <- edge (g ^. tileAt) g
    Just $ case edge' of
      North -> if c == cc then (r + 1, c) else pos
      South -> if c == cc then (r - 1, c) else pos
      West  -> if r == cr then (r, c + 1) else pos
      East  -> if r == cr then (r, c - 1) else pos

teleport :: Game -> Game
teleport g =
  foldl f g
    $ concatMap (\(p, t) -> zip (repeat p) (t ^. T.players))
    $ Map.toList
    $ Map.intersection (g ^. tiles) (g ^. gates)
 where
  f g' (from, p) = fromMaybe g' $ do
    op <- oppositeEdge from g'
    pd <- padding op g'
    -- TODO: refactor to use Linear.V2
    let to = (op ^. _1 + pd ^. _1, op ^. _2 + pd ^. _2)
    return $ walk' from to p g'

updateCurrentTilePosition :: Game -> Game
updateCurrentTilePosition g = g & tileAt .~ update (g ^. tileAt)
 where
  update pos@(r, c) = fromMaybe pos $ do
    edge' <- edge (g ^. tileAt) g
    Just $ case edge' of
      North -> (g ^. rowMax, c)
      South -> (g ^. rowMin, c)
      West  -> (r, g ^. colMax)
      East  -> (r, g ^. colMin)

nextPhase :: Game -> Game
nextPhase g = g & phase .~ nextPhase' (g ^. phase)
 where
  nextPhase' Plan = Walk
  nextPhase' _    = Plan

toggleGates :: Game -> Game
toggleGates g = g & gates .~ Map.mapWithKey toggleGate (g ^. gates)
 where
  toggleGate pos (Gate dir _) | pos == g ^. tileAt = Gate dir False
                              | otherwise          = Gate dir True

nextPlayer :: Game -> Game
nextPlayer g = g & player .~ P.next (g ^. player) (g ^. players)

--------------------------------------------------------------------------------
-- Plan phase
--------------------------------------------------------------------------------

rotate :: Game -> Game
rotate = rotateInternal T.rotate

rotate' :: Game -> Game
rotate' = rotateInternal T.rotate'

rotateInternal :: (Tile -> Tile) -> Game -> Game
rotateInternal r g = g & (tiles .~ Map.adjust r (g ^. tileAt) (g ^. tiles))

move :: Direction -> Game -> Game
move dir g = fromMoves moves' g where moves' = moves dir g

fromMoves :: [Position] -> Game -> Game
fromMoves []          g = g
fromMoves (newP : ps) g = fromMaybe (fromMoves ps g) $ do
  _ <- Map.lookup newP (g ^. gates)
  Just $ (updatePos . moveTile) g
 where
  moveTile  = tiles .~ Map.mapKeys moveKey (g ^. tiles)
  updatePos = tileAt .~ newP
  moveKey p | p == g ^. tileAt = newP
            | otherwise        = p

moves :: Direction -> Game -> [Position]
moves dir g = fromMaybe [] $ do
  e <- edge (g ^. tileAt) g
  let (r, c) = g ^. tileAt
      rMin   = g ^. rowMin
      rMax   = g ^. rowMax
      cMin   = g ^. colMin
      cMax   = g ^. colMax

  Just $ case (e, dir) of
    (North, East) ->
      [ (rMin, i) | i <- [c + 1 .. cMax] ]
        `List.union` [ (i, cMax) | i <- [rMin .. rMax] ]

    (North, West) ->
      [ (rMin, i) | i <- reverse [cMin .. c - 1] ]
        `List.union` [ (i, cMin) | i <- [rMin .. rMax] ]

    (South, East) ->
      [ (rMax, i) | i <- [c + 1 .. cMax] ]
        `List.union` [ (i, cMax) | i <- reverse [rMin .. rMax] ]

    (South, West) ->
      [ (rMax, i) | i <- reverse [cMin .. c - 1] ]
        `List.union` [ (i, cMin) | i <- reverse [rMin .. rMax] ]

    (West, North) ->
      [ (i, cMin) | i <- reverse [rMin .. r - 1] ]
        `List.union` [ (rMin, i) | i <- [cMin .. cMax] ]

    (West, South) ->
      [ (i, cMin) | i <- [r + 1 .. rMax] ]
        `List.union` [ (rMax, i) | i <- [cMin .. cMax] ]

    (East, North) ->
      [ (i, cMax) | i <- reverse [rMin .. r - 1] ]
        `List.union` [ (rMin, i) | i <- reverse [cMin .. cMax] ]

    (East, South) ->
      [ (i, cMax) | i <- [r + 1 .. rMax] ]
        `List.union` [ (rMax, i) | i <- reverse [cMin .. cMax] ]

    (North, South) -> [ (i, c) | i <- [r + 1 .. rMax] ]
    (West , East ) -> [ (r, i) | i <- [c + 1 .. cMax] ]
    (South, North) -> [ (i, c) | i <- reverse [rMin .. r - 1] ]
    (East , West ) -> [ (r, i) | i <- reverse [cMin .. c - 1] ]
    _              -> [] --  ¯\_(ツ)_/¯

--------------------------------------------------------------------------------
-- Walk phase
--------------------------------------------------------------------------------

walk :: Direction -> Game -> Game
walk d g = fromMaybe g $ do
  let player' = g ^. player
      tiles'  = g ^. tiles

  fromP <- playerPosition g (player' ^. P.color)
  let toP = step fromP d

  fromT <- Map.lookup fromP tiles'
  toT   <- Map.lookup toP tiles'

  let isConnected = T.connected d fromT toT
      isNotGate   = not $ isGate toP g
  guard (isConnected && isNotGate)

  return $ walk' fromP toP player' g

isGate :: Position -> Game -> Bool
isGate p g = fromMaybe False $ Map.lookup p (g ^. gates) >> return True

walk' :: Position -> Position -> Player -> Game -> Game
walk' from to player' g =
  g
    &  tiles
    .~ ( Map.alter (removePlayer player') from
       . Map.alter (insertPlayer player') to
       )
         (g ^. tiles)

insertPlayer :: Player -> Maybe Tile -> Maybe Tile
insertPlayer p mt = do
  t <- mt
  return $ t & T.players .~ (p : (t ^. T.players))

removePlayer :: Player -> Maybe Tile -> Maybe Tile
removePlayer p mt = do
  t <- mt
  return $ t & T.players .~ filter (/= p) (t ^. T.players)

step :: Position -> Direction -> Position
step (r, c) East  = (r, c + 1)
step (r, c) West  = (r, c - 1)
step (r, c) North = (r - 1, c)
step (r, c) South = (r + 1, c)

--------------------------------------------------------------------------------
-- etc
--------------------------------------------------------------------------------

edge :: Position -> Game -> Maybe Direction
edge (r, c) g | --  because we don't care about corners
                r == c             = Nothing
              | r == (g ^. rowMin) = Just North
              | r == (g ^. rowMax) = Just South
              | c == (g ^. colMin) = Just West
              | c == (g ^. colMax) = Just East
              | otherwise          = Nothing

oppositeEdge :: Position -> Game -> Maybe Position
oppositeEdge p@(r, c) g = do
  edge' <- edge p g
  return $ case edge' of
    North -> (g ^. rowMax, c)
    South -> (g ^. rowMin, c)
    East  -> (r, g ^. colMin)
    West  -> (r, g ^. colMax)

padding :: Position -> Game -> Maybe Position
padding p g = do
  edge' <- edge p g
  return $ case edge' of
    North -> (1, 0)
    South -> (-1, 0)
    East  -> (0, -1)
    West  -> (0, 1)

rowSpread :: Game -> [Int]
rowSpread = spread rowMin rowMax

colSpread :: Game -> [Int]
colSpread = spread colMin colMax

spread :: Lens' Game Int -> Lens' Game Int -> Game -> [Int]
spread mn mx g = [(g ^. mn) .. (g ^. mx)]

firstPlayer :: Players -> IO Player
firstPlayer p = fromJust <$> Random.choose (P.toList p)

playerMap :: Game -> Map Color (Position, Player)
playerMap g = foldl f mempty (Map.toList (g ^. tiles))
 where
  f m (p, t) = foldl (f' p) m (t ^. T.players)
  f' p m p' = Map.insert (p' ^. P.color) (p, p') m

playerPosition :: Game -> Color -> Maybe Position
playerPosition g c = fst <$> Map.lookup c (playerMap g)
