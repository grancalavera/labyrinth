{-# LANGUAGE TemplateHaskell #-}
module Labyrinth where

import Control.Lens

data Edge = North | West | South | East deriving (Eq, Show, Ord)

data Rotation = CW | CCW deriving (Eq, Show)

data TileKind = Gate | Path | Corner | Fork deriving (Eq, Show)

data Tile = Tile { _kind :: TileKind
                 , _edges :: [Edge]
                 , _coords :: Coords
                 } deriving (Eq)

data Coords = Coords { _x :: Int
                     , _y :: Int } deriving (Eq)

data Board = Board { _tiles :: [Tile]
                   , _cols :: Int
                   } deriving (Eq)

makeLenses ''Tile
makeLenses ''Coords
makeLenses ''Board

instance Show Tile where
  show t =    "{"
           ++ (show $ _kind t)
           ++ ","
           ++ show (_edges t)
           ++ ","
           ++ (show $ _coords t)
           ++ "}"

instance Show Coords where
  show c =    "("
           ++ (show $ view x c)
           ++ ","
           ++ (show $ view y c)
           ++ ")"

coordsFromIndex :: Int -> Int -> Coords
coordsFromIndex cols index = Coords { _x = x, _y = y}
  where x = index `mod` cols
        y = index `div` cols

rotateEdge :: Rotation -> Edge -> Edge
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

rotateTimes :: Int  -> Tile  -> Tile
rotateTimes n = foldr (\r -> \r' -> r.r') id (replicate n (rotate CCW))

makeTile :: TileKind -> Int -> Int -> Tile
makeTile tileKind x y = Tile { _coords = Coords {_x = x, _y = y}
                             , _kind = tileKind
                             , _edges = tileEdges
                             }
  where tileEdges = case tileKind of
                      Gate         -> [North]
                      Path -> [North, South]
                      Corner   -> [North, West]
                      Fork     -> [West, North, East]

gate   = makeTile Gate
path   = makeTile Path
corner = makeTile Corner
fork   = makeTile Fork

-- utilities and stuff

splitAll :: Int -> [a] -> [[a]]
splitAll at xs = split (splitAt at xs)
  where split :: ([a], [a]) -> [[a]]
        split ([], _) = []
        split (xs', ys) = xs' : (splitAll at ys)
