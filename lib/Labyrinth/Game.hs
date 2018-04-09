{-# LANGUAGE NamedFieldPuns  #-}
{-# LANGUAGE RankNTypes      #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module Labyrinth.Game
  (
  -- Working with Games
    Game(..)
  , tileAt
  , token
  , phase
  , rows
  , cols
  , players
  , gates
  , tiles
  , treasureMap
  , fromDescription
  , lastRow
  , lastColumn
  , player
  , goal

  -- State transitions
  , Phase(..)
  , done

  -- Game play
  , moveTile
  , rotateTile
  , rotateTile'
  , moveToken
  )
where

import           Control.Monad                  ( guard )
import qualified Data.List.Extended            as L
import qualified Data.Tuple                    as Tu
import qualified Data.Set                      as Set
import qualified Data.Map.Strict               as Map
import           Data.Map.Strict                ( Map )
import           Data.Maybe                     ( fromJust
                                                , fromMaybe
                                                )
import           Linear.V2                      ( V2(..) )
import           Labyrinth.Position             ( Position )
import           Labyrinth.Direction            ( Direction(..) )
import           Labyrinth.Game.Description     ( DGame(..) )
import qualified Labyrinth.Game.Description    as GD
import           Labyrinth.Gate                 ( Gate(..) )
import           Labyrinth.Treasure             ( Search
                                                , Found
                                                )
import qualified Labyrinth.Players             as P
import           Labyrinth.Players              ( Color(..)
                                                , Players
                                                , Player
                                                )
import           Labyrinth.Tile                 ( Tile(..) )
import qualified Labyrinth.Tile                as T
import           Lens.Micro                     ( (&)
                                                , (.~)
                                                , (^.)
                                                )
import           Lens.Micro.TH                  ( makeLenses )
import           Lens.Micro.Type                ( Lens' )

data Phase = Plan | Search | Over deriving (Show, Eq)

data Game = Game
    { _tileAt      :: Position
    , _token     :: Color
    , _phase       :: Phase
    , _rows        :: [Int]
    , _cols        :: [Int]
    , _players     :: Players
    , _gates       :: Map Position Gate
    , _tiles       :: Map Position Tile
    , _treasureMap :: Map Color ([Search], [Found])
    } deriving (Show, Eq)
makeLenses ''Game

fromDescription :: DGame -> IO Game
fromDescription gd = do

  tiles'       <- GD.tiles gd
  token'       <- GD.firstToken gd
  treasureMap' <- GD.treasureMap gd

  return Game
    { _tileAt      = gd ^. GD.gStartPosition
    , _token       = token'
    , _phase       = Plan
    , _rows        = GD.rows gd
    , _cols        = GD.cols gd
    , _gates       = GD.gates gd
    , _tiles       = tiles'
    , _players     = gd ^. GD.gPlayers
    , _treasureMap = treasureMap'
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
        . teleportToken
        . toggleGates
        . updateCurrentTilePosition
        . slideTile
        )
          g
  _    -> (nextPhase . nextToken) g

slideTile :: Game -> Game
slideTile g = g & tiles .~ Map.mapKeys slide (g ^. tiles)
 where
  tilePos = g ^. tileAt
  sameR   = sameRow tilePos
  sameC   = sameColumn tilePos

  slide p = fromMaybe p $ do
    edge' <- edge tilePos g
    Just $ case edge' of
      North -> if sameC p then nudgeSouth p else p
      West  -> if sameR p then nudgeEast p else p
      South -> if sameC p then nudgeNorth p else p
      East  -> if sameR p then nudgeWest p else p

teleportToken :: Game -> Game
teleportToken g =
  foldl f g
    $ concatMap (\(p, t) -> zip (repeat p) (T.tokenList t))
    $ Map.toList
    $ Map.intersection (g ^. tiles) (g ^. gates)
 where
  f g' (from, p) = fromMaybe g' $ do
    op <- oppositeEdge from g'
    to <- pad op g'
    return $ walk from to p g'

updateCurrentTilePosition :: Game -> Game
updateCurrentTilePosition g = g & tileAt .~ update (g ^. tileAt)
 where
  update pos@(V2 r c) = fromMaybe pos $ do
    edge' <- edge (g ^. tileAt) g
    Just $ case edge' of
      North -> V2 (lastRow g) c
      West  -> V2 r (lastColumn g)
      South -> V2 0 c
      East  -> V2 r 0

lastColumn :: Game -> Int
lastColumn = far cols

lastRow :: Game -> Int
lastRow = far rows

far :: Lens' Game [Int] -> Game -> Int
far l g = fromMaybe 0 $ L.safeLast $ g ^. l

nextPhase :: Game -> Game
nextPhase g = g & phase .~ nextPhase' (g ^. phase)
 where
  nextPhase' Plan = Search
  nextPhase' _    = Plan

toggleGates :: Game -> Game
toggleGates g = g & gates .~ Map.mapWithKey toggleGate (g ^. gates)
 where
  toggleGate pos (Gate dir _) | pos == g ^. tileAt = Gate dir False
                              | otherwise          = Gate dir True

--------------------------------------------------------------------------------
-- Plan phase
--------------------------------------------------------------------------------

moveTile :: Direction -> Game -> Game
moveTile dir g = chooseMove moves' g where moves' = moves dir g

rotateTile :: Game -> Game
rotateTile = rotate T.rotate

rotateTile' :: Game -> Game
rotateTile' = rotate T.rotate'

rotate :: (Tile -> Tile) -> Game -> Game
rotate r g = g & (tiles .~ Map.adjust r (g ^. tileAt) (g ^. tiles))

chooseMove :: [Position] -> Game -> Game
chooseMove []          g = g
chooseMove (newP : ps) g = fromMaybe (chooseMove ps g) $ do
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
      cMax   = lastColumn g
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
-- Search phase
--------------------------------------------------------------------------------

moveToken :: Direction -> Game -> Game
moveToken d = search . moveToken' d

search :: Game -> Game
search = id

moveToken' :: Direction -> Game -> Game
moveToken' d g = fromMaybe g $ do
  let token' = g ^. token
      tiles' = g ^. tiles

  fromP <- tokenPosition g token'
  let toP = nudge d fromP

  fromT <- Map.lookup fromP tiles'
  toT   <- Map.lookup toP tiles'

  let isConnected = T.connected d fromT toT
      isNotGate   = not $ isGate toP g
  guard (isConnected && isNotGate)

  return $ walk fromP toP token' g

--------------------------------------------------------------------------------
-- etc
--------------------------------------------------------------------------------

walk :: Position -> Position -> Color -> Game -> Game
walk from to c g = g & tiles .~ (grabToken . placeToken) (g ^. tiles)
 where
  grabToken  = Map.alter (withToken (Set.delete c)) from
  placeToken = Map.alter (withToken (Set.insert c)) to
  withToken f mt = do
    t <- mt
    return $ t & T.tokens .~ f (t ^. T.tokens)

isGate :: Position -> Game -> Bool
isGate p g = fromMaybe False $ Map.lookup p (g ^. gates) >> return True

nudge :: Direction -> Position -> Position
nudge d = (nudge' +)
 where
  nudge' = case d of
    North -> V2 (-1) 0
    West  -> V2 0 (-1)
    South -> V2 1 0
    East  -> V2 0 1

nudgeNorth, nudgeWest, nudgeSouth, nudgeEast :: Position -> Position
nudgeNorth = nudge North
nudgeWest = nudge West
nudgeSouth = nudge South
nudgeEast = nudge East

sameRow, sameColumn :: Position -> Position -> Bool
sameRow (V2 r _) (V2 r' _) = r == r'
sameColumn (V2 _ c) (V2 _ c') = c == c'

edge :: Position -> Game -> Maybe Direction
edge (V2 r c) g | --  because we don't care about corners
                  r == c            = Nothing
                | r == 0            = Just North
                | r == lastRow g    = Just South
                | c == 0            = Just West
                | c == lastColumn g = Just East
                | otherwise         = Nothing

oppositeEdge :: Position -> Game -> Maybe Position
oppositeEdge pos@(V2 r c) g = do
  edge' <- edge pos g
  return $ case edge' of
    North -> V2 (lastColumn g) c
    West  -> V2 r (lastRow g)
    South -> V2 0 c
    East  -> V2 r 0

pad :: Position -> Game -> Maybe Position
pad p g = do
  edge' <- edge p g
  return $ ($ p) $ case edge' of
    North -> nudgeSouth
    West  -> nudgeEast
    South -> nudgeNorth
    East  -> nudgeWest

tokenPosition :: Game -> Color -> Maybe Position
tokenPosition g c = Map.lookup c (positionByColor g)

positionByColor :: Game -> Map Color Position
positionByColor g =
  Map.fromList
    $ map Tu.swap
    $ concatMap (\(p, t) -> zip (repeat p) (T.tokenList t))
    $ filter (not . null . (^. T.tokens) . snd) -- this line might be redundant
    $ Map.toList (g ^. tiles)

nextToken :: Game -> Game
nextToken g = g & token .~ (cycle ts !! (i + 1))
 where
  ts = tokens g
  i  = fromJust $ L.elemIndex (g ^. token) ts

tokens :: Game -> [Color]
tokens g = Map.keys $ P.toMap (g ^. players)

player :: Game -> Player
player g = fromJust $ Map.lookup (g ^. token) (P.toMap $ g ^. players)

goal :: Game -> Maybe Search
goal g = Map.lookup (g ^. token) (g ^. treasureMap) >>= (L.safeHead . fst)
