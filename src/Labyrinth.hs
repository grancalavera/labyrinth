{-# LANGUAGE TemplateHaskell #-}
module Labyrinth where
import Data.List (intercalate)
import Control.Monad (liftM)
import Control.Lens

data Direction =   North
                 | West
                 | South
                 | East deriving (Eq)

instance Show Direction where
  show North = "N"
  show West  = "W"
  show South = "S"
  show East  = "E"

data Rotation =   CW
                | CCW deriving (Eq, Show)

data TileType =   Border
                | Gate
                | Path
                | Corner
                | Fork deriving (Eq)

instance Show TileType where
  show Border = "B"
  show Gate   = "G"
  show Path   = "P"
  show Corner = "C"
  show Fork   = "F"

data Tile = Tile { _kind :: TileType
                 , _edges :: [Direction]
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
  show t =     "["
            ++ (show $ _kind t)
            ++ pad ' ' 3 (concatMap show (_edges t))
            ++ (show $ _coords t)
            ++ "]"

instance Show Coords where
  show c =    "("
           ++ pad ' ' 2 (show $ view x c)
           ++ ","
           ++ pad ' ' 2 (show $ view y c)
           ++ ")"

coordsFromIndex :: Int -> Int -> Coords
coordsFromIndex cols index = Coords { _x = x, _y = y}
  where x = index `mod` cols
        y = index `div` cols

makeCoords :: Int -> Int -> Coords
makeCoords x y = Coords {_x = x, _y = y}

rotate :: Rotation -> Direction -> Direction
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

makeTile :: TileType -> Coords -> Tile
makeTile Border c = Tile { _coords = c
                         , _kind = Border
                         , _edges = []}
makeTile Gate c   = Tile { _coords = c
                         , _kind = Gate
                         , _edges = [North]}
makeTile Path c   = Tile { _coords = c
                         , _kind = Path
                         , _edges = [North, South]}
makeTile Corner c = Tile { _coords = c
                         , _kind = Corner
                         , _edges = [North, West]}
makeTile Fork c   = Tile { _coords = c
                         , _kind = Fork
                         , _edges = [West, North, East]}

makeBorder = makeTile Border
makeGate   = makeTile Gate
makePath   = makeTile Path
makeCorner = makeTile Corner
makeFork   = makeTile Fork

gateS = makeTile Gate (makeCoords 0 0)
gateE = rotateTileOnce gateS
gateN = rotateTileOnce gateE
gateW = rotateTileOnce gateN

pathNS = makeTile Path (makeCoords 0 0)
pathWE = rotateTileOnce pathNS

cornerNW = makeTile Corner (makeCoords 0 0)
cornerSW = rotateTileOnce cornerNW
cornerSE = rotateTileOnce cornerSW
cornerNE = rotateTileOnce cornerSE

forkNWE = makeTile Fork (makeCoords 0 0)
forkNWS = rotateTileOnce forkNWE
forkSWE = rotateTileOnce forkNWS
forkNES = rotateTileOnce forkSWE

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

board = Board { _cols=9
              , _tiles=[makeBorder (coordsFromIndex 9 i) | i <- [0..9*9-1]]
              }
