{-# LANGUAGE TemplateHaskell #-}
module Labyrinth where
import Data.List (intercalate)
import Control.Monad (liftM)
import Control.Lens

data Edge =   North
            | West
            | South
            | East deriving (Eq, Show, Ord)

data Rotation =   CW
                | CCW deriving (Eq, Show)

data TileKind =   Border
                | Corner
                | Gate
                | StraightPath
                | CornerPath
                | ForkPath deriving (Eq, Show)

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

instance Show Board where
  show board = showBoard (view cols board) (view tiles board)

instance Show Tile where
  show t =     "{"
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

makeCoords :: Int -> Int -> Coords
makeCoords x y = Coords {_x = x, _y = y}

rotate :: Rotation -> Edge -> Edge
rotate CW  North  = West
rotate CW  West   = South
rotate CW  South  = East
rotate CW  East   = North
rotate CCW North  = East
rotate CCW West   = North
rotate CCW South  = West
rotate CCW East   = South

rotateTile :: Rotation -> Tile -> Tile
rotateTile r = over (edges . traverse) (rotate r)

rotateTileOnce :: Tile -> Tile
rotateTileOnce = rotateTile CW

rotateTileTwice :: Tile -> Tile
rotateTileTwice = rotateTileOnce . rotateTileOnce

rotateTileThrice :: Tile -> Tile
rotateTileThrice = rotateTileOnce . rotateTileTwice

makeTile :: TileKind -> Int -> Int -> Tile
makeTile tileKind x y = Tile { _coords = makeCoords x y
                             , _kind = tileKind
                             , _edges = tileEdges
                             }
  where tileEdges = case tileKind of
                      Border       -> [North]
                      Corner       -> [North, West]
                      Gate         -> [North]
                      StraightPath -> [North, South]
                      CornerPath   -> [North, West]
                      ForkPath     -> [West, North, East]

makeBorder       = makeTile Border
makeCorner       = makeTile Corner
makeGate         = makeTile Gate
makeStraightPath = makeTile StraightPath
makeCornerPath   = makeTile CornerPath
makeForkPath     = makeTile ForkPath

-- utilities and stuff

splitAll :: Int -> [a] -> [[a]]
splitAll at xs = split (splitAt at xs)
  where split :: ([a], [a]) -> [[a]]
        split ([], _) = []
        split (xs', ys) = xs' : (splitAll at ys)

pad :: Char -> Int -> String -> String
pad p w x = pad' x
  where pad' s | length s > w = ""
               | length s == w = s
               | otherwise = pad' ([p] ++ s)

showBoard :: Int -> [Tile] -> String
showBoard cols tiles = intercalate "\n"
    $ liftM (intercalate "  ")
      $ splitAll 9
        $ map show tiles
