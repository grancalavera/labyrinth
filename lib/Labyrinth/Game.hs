{-# LANGUAGE NamedFieldPuns  #-}
{-# LANGUAGE RankNTypes      #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module Labyrinth.Game
  (
  -- Working with Games
    Game(..)
  , fromDescription
  , tiles
  , tileAt
  , gates
  , phase
  , player
  , players
  , rows
  , cols
  , lastRow
  , lastColumn

  -- State transitions
  , Phase(..)
  , done

  -- Game play
  , moveTile
  , rotateTile
  , rotateTile'
  , movePlayer
  )
where

import           Control.Monad                  ( guard )
import qualified Data.List.Extended            as L
import           Data.Map.Strict                ( Map )
import qualified Data.Map.Strict               as Map
import           Data.Maybe                     ( fromJust
                                                , fromMaybe
                                                )
import           Linear.V2                      ( V2(..) )
import           Labyrinth.Position             ( Position )
import qualified Labyrinth.Random              as Random
import           Labyrinth.Direction            ( Direction(..) )
import           Labyrinth.Game.Description     ( DGame(..) )
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
                                                )
import           Lens.Micro.TH                  ( makeLenses )
import           Lens.Micro.Type                ( Lens' )

data Phase = Plan | Walk | End deriving (Show, Eq)

data Game = Game
    { _tileAt  :: Position
    , _tiles   :: Map Position Tile
    , _gates   :: Map Position Gate
    , _phase   :: Phase
    , _rows    :: [Int]
    , _cols    :: [Int]
    , _player  :: Player
    , _players :: Players
    } deriving (Show, Eq)
makeLenses ''Game

fromDescription :: DGame -> IO Game
fromDescription gd = do

  player' <- firstPlayer (gd ^. GD.gPlayers)
  tiles'  <- GD.mkTiles gd

  return Game
    { _tileAt  = gd ^. GD.gStartPosition
    , _player  = player'
    , _players = gd ^. GD.gPlayers
    , _gates   = Map.fromList $ gd ^. GD.gGates
    , _tiles   = Map.fromList tiles'
    , _phase   = Plan
    , _rows    = GD.rows gd
    , _cols    = GD.cols gd
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
        . teleportPlayer
        . toggleGates
        . updateCurrentTilePosition
        . slideTile
        )
          g
  _ -> (nextPhase . nextPlayer) g

slideTile :: Game -> Game
slideTile g = g & tiles .~ Map.mapKeys slide (g ^. tiles)
 where
  V2 cr cc = g ^. tileAt
  slide pos@(V2 r c) = fromMaybe pos $ do
    edge' <- edge (g ^. tileAt) g
    Just $ case edge' of
      North -> if c == cc then V2 (r + 1) c else pos
      South -> if c == cc then V2 (r - 1) c else pos
      West  -> if r == cr then V2 r (c + 1) else pos
      East  -> if r == cr then V2 r (c - 1) else pos

teleportPlayer :: Game -> Game
teleportPlayer g =
  foldl f g
    $ concatMap (\(p, t) -> zip (repeat p) (t ^. T.players))
    $ Map.toList
    $ Map.intersection (g ^. tiles) (g ^. gates)
 where
  f g' (from, p) = fromMaybe g' $ do
    op <- oppositeEdge from g'
    pd <- padding op g'
    let to = op + pd
    return $ walk from to p g'

updateCurrentTilePosition :: Game -> Game
updateCurrentTilePosition g = g & tileAt .~ update (g ^. tileAt)
 where
  update pos@(V2 r c) = fromMaybe pos $ do
    edge' <- edge (g ^. tileAt) g
    Just $ case edge' of
      North -> V2 (lastRow g) c
      South -> V2 0 c
      West  -> V2 r (lastColumn  g)
      East  -> V2 r 0

lastColumn  :: Game -> Int
lastColumn  = far cols

lastRow :: Game -> Int
lastRow = far rows

far :: Lens' Game [Int] -> Game -> Int
far l g = fromMaybe 0 $ L.safeLast $ g ^. l

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

moveTile :: Direction -> Game -> Game
moveTile dir g = fromMoves moves' g where moves' = moves dir g

rotateTile :: Game -> Game
rotateTile = rotate T.rotate

rotateTile' :: Game -> Game
rotateTile' = rotate T.rotate'

rotate :: (Tile -> Tile) -> Game -> Game
rotate r g = g & (tiles .~ Map.adjust r (g ^. tileAt) (g ^. tiles))

fromMoves :: [Position] -> Game -> Game
fromMoves []          g = g
fromMoves (newP : ps) g = fromMaybe (fromMoves ps g) $ do
  _ <- Map.lookup newP (g ^. gates)
  Just $ (updatePos . moveTile') g
 where
  moveTile' = tiles .~ Map.mapKeys moveKey (g ^. tiles)
  updatePos = tileAt .~ newP
  moveKey p | p == g ^. tileAt = newP
            | otherwise        = p

moves :: Direction -> Game -> [Position]
moves dir g = fromMaybe [] $ do
  edge' <- edge (g ^. tileAt) g
  let V2 r c = g ^. tileAt
      rMin   = 0
      rMax   = lastRow g
      cMin   = 0
      cMax   = lastColumn  g
  Just $ case (edge', dir) of
    (North, East) ->
      [ V2 rMin i | i <- [c + 1 .. cMax] ]
        `L.union` [ V2 i cMax | i <- [rMin .. rMax] ]

    (North, West) ->
      [ V2 rMin i | i <- reverse [cMin .. c - 1] ]
        `L.union` [ V2 i cMin | i <- [rMin .. rMax] ]

    (South, East) ->
      [ V2 rMax i | i <- [c + 1 .. cMax] ]
        `L.union` [ V2 i cMax | i <- reverse [rMin .. rMax] ]

    (South, West) ->
      [ V2 rMax i | i <- reverse [cMin .. c - 1] ]
        `L.union` [ V2 i cMin | i <- reverse [rMin .. rMax] ]

    (West, North) ->
      [ V2 i cMin | i <- reverse [rMin .. r - 1] ]
        `L.union` [ V2 rMin i | i <- [cMin .. cMax] ]

    (West, South) ->
      [ V2 i cMin | i <- [r + 1 .. rMax] ]
        `L.union` [ V2 rMax i | i <- [cMin .. cMax] ]

    (East, North) ->
      [ V2 i cMax | i <- reverse [rMin .. r - 1] ]
        `L.union` [ V2 rMin i | i <- reverse [cMin .. cMax] ]

    (East, South) ->
      [ V2 i cMax | i <- [r + 1 .. rMax] ]
        `L.union` [ V2 rMax i | i <- reverse [cMin .. cMax] ]

    (North, South) -> [ V2 i c | i <- [r + 1 .. rMax] ]
    (West , East ) -> [ V2 r i | i <- [c + 1 .. cMax] ]
    (South, North) -> [ V2 i c | i <- reverse [rMin .. r - 1] ]
    (East , West ) -> [ V2 r i | i <- reverse [cMin .. c - 1] ]
    _              -> [] --  ¯\_(ツ)_/¯

--------------------------------------------------------------------------------
-- Walk phase
--------------------------------------------------------------------------------

movePlayer :: Direction -> Game -> Game
movePlayer d g = fromMaybe g $ do
  let player' = g ^. player
      tiles'  = g ^. tiles

  fromP <- playerPosition g (player' ^. P.color)
  let toP = step fromP d

  fromT <- Map.lookup fromP tiles'
  toT   <- Map.lookup toP tiles'

  let isConnected = T.connected d fromT toT
      isNotGate   = not $ isGate toP g
  guard (isConnected && isNotGate)

  return $ walk fromP toP player' g

isGate :: Position -> Game -> Bool
isGate p g = fromMaybe False $ Map.lookup p (g ^. gates) >> return True

walk :: Position -> Position -> Player -> Game -> Game
walk from to player' g =
  g
    &  tiles
    .~ (Map.alter (removePlayer player') from . Map.alter (insertPlayer player') to)
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
step (V2 r c) East  = V2 r (c + 1)
step (V2 r c) West  = V2 r (c - 1)
step (V2 r c) North = V2 (r - 1) c
step (V2 r c) South = V2 (r + 1) c

--------------------------------------------------------------------------------
-- etc
--------------------------------------------------------------------------------

edge :: Position -> Game -> Maybe Direction
edge (V2 r c) g | --  because we don't care about corners
                  r == c        = Nothing
                | r == 0        = Just North
                | r == lastRow g = Just South
                | c == 0        = Just West
                | c == lastColumn  g = Just East
                | otherwise     = Nothing

oppositeEdge :: Position -> Game -> Maybe Position
oppositeEdge pos@(V2 r c) g = do
  edge' <- edge pos g
  return $ case edge' of
    North -> V2 (lastRow g) c
    South -> V2 0 c
    East  -> V2 r 0
    West  -> V2 r (lastColumn  g)

-- after refactoring this can be just a multiplication
padding :: Position -> Game -> Maybe Position
padding p g = do
  edge' <- edge p g
  return $ case edge' of
    North -> V2 1 0
    South -> V2 (-1) 0
    East  -> V2 0 (-1)
    West  -> V2 0 1

firstPlayer :: Players -> IO Player
firstPlayer p = fromJust <$> Random.choose (P.toList p)

playerMap :: Game -> Map Color (Position, Player)
playerMap g = foldl f mempty (Map.toList (g ^. tiles))
 where
  f m (p, t) = foldl (f' p) m (t ^. T.players)
  f' p m p' = Map.insert (p' ^. P.color) (p, p') m

playerPosition :: Game -> Color -> Maybe Position
playerPosition g c = fst <$> Map.lookup c (playerMap g)
