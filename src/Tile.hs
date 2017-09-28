{-# LANGUAGE TemplateHaskell #-}
module Tile where
import Control.Lens

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

coordsFromIndex :: Int -> Int -> Coords
coordsFromIndex cols index = Coords { _x = x, _y = y}
  where x = index `mod` cols
        y = index `div` cols

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
rotate r = over (edges . traverse) (rotateEdge r)

move :: Direction -> Tile -> Tile
move North = over (coords.y) (\n -> n-1)
move South = over (coords.y) (+1)
move West  = over (coords.x) (\n -> n-1)
move East  = over (coords.x) (+1)

up :: Tile -> Tile
up    = move North
down  = move South
left  = move West
right = move East

rotateTimes :: Int  -> Tile  -> Tile
rotateTimes n = foldr (\r -> \r' -> r.r') id (replicate n (rotate CCW))

makeTile :: TileKind -> Int -> Int -> Tile
makeTile tileKind x y = Tile { _coords = Coords {_x = x, _y = y}
                             , _kind = tileKind
                             , _edges = tileEdges
                             }
  where tileEdges = case tileKind of
                      Gate   -> [North]
                      Path   -> [North, South]
                      Corner -> [North, West]
                      Fork   -> [West, North, East]

gate   = makeTile Gate
path   = makeTile Path
corner = makeTile Corner
fork   = makeTile Fork
