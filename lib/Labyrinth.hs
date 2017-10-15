{-# LANGUAGE TemplateHaskell #-}
module Labyrinth where

import System.Random (randomRIO)
import Lens.Micro ((.~), (%~), (^.), (^..), (&))
import Lens.Micro.TH (makeLenses)

-- &    reverse application operator
-- .~   set
-- %~   over
-- ^.   view
-- ^..  traverse

data Direction = North | West | South | East deriving (Eq, Show, Ord)
data Rotation = CW | CCW deriving (Eq, Show)
data TileKind = Gate | Path | Corner | Fork deriving (Eq, Show)

data Tile = Tile { _kind :: TileKind
                 , _edges :: [Direction]
                 , _coords :: Coords
                 } deriving (Show)

data Coords = Coords { _x :: Int, _y :: Int } deriving (Show)

makeLenses ''Tile
makeLenses ''Coords

data Board = Board { _tiles :: [Tile]
                   , _cols :: Int
                   , _rows :: Int
                   } deriving (Show)

makeLenses ''Board

coordsFromIndex :: Int -> Int -> Coords
coordsFromIndex cls i = Coords { _x = i `mod` cls
                               , _y = i `div` cls
                               }

rotateEdge :: Rotation -> Direction -> Direction
rotateEdge CCW North  = West
rotateEdge CCW West   = South
rotateEdge CCW South  = East
rotateEdge CCW East   = North
rotateEdge CW  North  = East
rotateEdge CW  West   = North
rotateEdge CW  South  = West
rotateEdge CW  East   = South

rotate :: Rotation -> Tile -> Tile
rotate r = edges . traverse %~ (rotateEdge r)

move :: Direction -> Tile -> Tile
move North = coords . y %~ (\n -> n-1)
move South = coords . y %~ (+1)
move West  = coords . x %~ (\n -> n-1)
move East  = coords . x %~ (+1)

up, down, left, right :: Tile -> Tile
up    = move North
down  = move South
left  = move West
right = move East

rotateTimes :: Int  -> Tile  -> Tile
rotateTimes n = foldr (\r -> \r' -> r.r') id (replicate n (rotate CCW))

makeTile :: TileKind -> Int -> Int -> Tile
makeTile tileKind tx ty = Tile { _coords = Coords {_x = tx, _y = ty}
                               , _kind = tileKind
                               , _edges = tileEdges
                               }
  where tileEdges = case tileKind of
                      Gate   -> [North]
                      Path   -> [North, South]
                      Corner -> [North, West]
                      Fork   -> [West, North, East]

gate, path, corner, fork :: Int -> Int -> Tile
gate   = makeTile Gate
path   = makeTile Path
corner = makeTile Corner
fork   = makeTile Fork

initialBoard :: IO Board
initialBoard = do
  shufledTiles' <- shuffledTiles
  return Board { _rows = 9
               , _cols = 9
               , _tiles = fixedTiles ++ shufledTiles'
               }

sillyBoard :: Board
sillyBoard = Board { _rows = 2
                   , _cols = 2
                   , _tiles = [ rotateTimes 2 $ corner 0 0
                              , rotateTimes 1 $ corner 1 0
                              , rotateTimes 3 $ corner 0 1
                              , corner 1 1
                              ]
                   }

fixedTiles :: [Tile]
fixedTiles =    []

             ++ map (\x' -> rotateTimes 2 $ gate x' 0) gateStops
             ++ map (\x' ->                 gate x' 8) gateStops
             ++ map (\y' -> rotateTimes 3 $ gate 0 y') gateStops
             ++ map (\y' -> rotateTimes 1 $ gate 8 y') gateStops

             ++ [rotateTimes 2 $ corner 1 1,
                 rotateTimes 1 $ corner 7 1,
                 rotateTimes 3 $ corner 1 7,
                                 corner 7 7]

             ++ map (\x' -> rotateTimes 2 $ fork x' 1) forkStops
             ++ map (\x' ->                 fork x' 7) forkStops
             ++ map (\y' -> rotateTimes 3 $ fork 1 y') forkStops
             ++ map (\y' -> rotateTimes 1 $ fork 7 y') forkStops

             ++ [rotateTimes 3 $ fork 3 3,
                                 fork 3 5,
                 rotateTimes 2 $ fork 5 3,
                 rotateTimes 1 $ fork 5 5]

shuffledTiles :: IO [Tile]
shuffledTiles = do
  tiles' <- shuffleList shuffledTilesTiles
  (t:ts) <- mapM rotateTileRandomly tiles'
  return (
    [t & (coords.x) .~ 2] ++
    map (\(c, t') -> t' & coords .~ c) (zip shuffledTileStops ts)
    )

borderStops :: [Int]
borderStops = [1,3,5,7]

gateStops :: [Int]
gateStops = [2,4,6]

forkStops :: [Int]
forkStops = [3, 5]

shuffledTileStops :: [Coords]
shuffledTileStops =    []
                    ++ [Coords{_x=x',_y=y'} | x' <- [2,4,6], y' <- [1,3,5,7]]
                    ++ [Coords{_x=x',_y=y'} | x' <- [1..7], y' <- [2,4,6]]

shuffledTilesTiles :: [Tile]
shuffledTilesTiles =    []
                     ++ (replicate 12 $ path 0 0)
                     ++ (replicate 16 $ corner 0 0)
                     ++ (replicate 6  $ fork 0 0)

shuffleList :: [a] -> IO [a]
shuffleList [] = return []
shuffleList list = do
  i <- randomRIO (0, (length list)-1)
  case (splitAt i list) of
    (before, (hd:after)) -> do
      tl <- shuffleList (before ++ after)
      return (hd:tl)
    _ -> return []

randomRotation :: IO (Tile -> Tile)
randomRotation = do
  n <- randomRIO (0, 3)
  return (rotateTimes n)

rotateTileRandomly :: Tile -> IO Tile
rotateTileRandomly tile = do
  f <- randomRotation
  return (f tile)

toListOfX, toListOfY :: Board -> [Int]
toListOfX b = b ^.. (tiles.traverse.coords.x)
toListOfY b = b ^.. (tiles.traverse.coords.y)

toListOfCoords :: Board -> [Coords]
toListOfCoords b = b ^.. (tiles.traverse.coords)

overCoords :: (Coords -> Coords) -> Board -> Board
overCoords = (tiles . traverse . coords %~)

moveCoordsBy :: Coords -> Coords -> Coords
moveCoordsBy byCoords fromCoords = Coords {_x = newX, _y = newY}
  where newX = fromCoords ^. x + byCoords ^. x
        newY = fromCoords ^. y + byCoords ^. y
