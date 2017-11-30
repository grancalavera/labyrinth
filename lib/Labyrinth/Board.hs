module Labyrinth.Board
    ( Board
    , Position
    , fixedTiles
    , toList
    , shuffle
    ) where

import           System.Random    (randomRIO)
import           Control.Monad    (forM)
import qualified Data.Array.IO    as ArrayIO
import           Data.Array.IO    (IOArray)
import qualified Data.Map         as Map
import           Data.Map         (Map)
import           Data.Monoid      ((<>))
import           Labyrinth.Tile   (Tile(..), Terrain(..), Direction(..))

type Position = (Int, Int)
data Cell a = Cell Position a
data Board a = Board (Map Position a) deriving (Show, Eq)

instance Monoid (Board a) where
  mempty = Board mempty
  Board l `mappend` Board r = Board (Map.union r l)

toList :: Board a -> [(Position, a)]
toList (Board m) = Map.toList m

fromCell :: Cell a -> Board a
fromCell (Cell p x) = Board (Map.insert p x mempty)

fromCells :: [Cell a] -> Board a
fromCells cs = foldl (<>) mempty (map fromCell cs)

fixedTiles :: Board Tile
fixedTiles = fromCells
  [ Cell (2, 0) (Tile Gate South)
  , Cell (4, 0) (Tile Gate South)
  , Cell (6, 0) (Tile Gate South)
  , Cell (0, 2) (Tile Gate East)
  , Cell (0, 4) (Tile Gate East)
  , Cell (0, 6) (Tile Gate East)
  , Cell (8, 2) (Tile Gate West)
  , Cell (8, 4) (Tile Gate West)
  , Cell (8, 6) (Tile Gate West)
  , Cell (2, 8) (Tile Gate North)
  , Cell (4, 8) (Tile Gate North)
  , Cell (6, 8) (Tile Gate North)
  , Cell (1, 1) (Tile Corner South)
  , Cell (7, 1) (Tile Corner West)
  , Cell (1, 7) (Tile Corner East)
  , Cell (7, 7) (Tile Corner North)
  , Cell (3, 1) (Tile Fork South)
  , Cell (5, 1) (Tile Fork South)
  , Cell (1, 3) (Tile Fork East)
  , Cell (1, 5) (Tile Fork East)
  , Cell (7, 3) (Tile Fork West)
  , Cell (7, 5) (Tile Fork West)
  , Cell (3, 7) (Tile Fork North)
  , Cell (5, 7) (Tile Fork North)
  , Cell (3, 3) (Tile Fork East)
  , Cell (5, 3) (Tile Fork South)
  , Cell (3, 5) (Tile Fork North)
  , Cell (5, 5) (Tile Fork West)
  ]

movableTiles :: [Tile]
movableTiles =
  replicate 12 (Tile Path North) ++
  replicate 16 (Tile Corner North) ++
  replicate 6  (Tile Fork North)

movablePositions :: [Position]
movablePositions =
  [(x,y) | x <- [2,4,6], y <- [1,3,5,7]] ++
  [(x,y) | x <- [1..7], y <- [2, 4, 6]]

shuffle :: [a] -> IO [a]
shuffle xs = do
  ar <- newArray n xs
  forM [1..n] $ \i -> do
    j <- randomRIO (i, n)
    vi <- ArrayIO.readArray ar i
    vj <- ArrayIO.readArray ar j
    ArrayIO.writeArray ar j vi
    return vj
  where
    n = length xs
    newArray :: Int -> [a] -> IO (IOArray Int a)
    newArray n xs' = ArrayIO.newListArray (1, n) xs'
