module Labyrinth
  ( shuffle
  , rotateRandom
  ) where

import qualified Data.Array.IO  as AIO
import           Data.Array.IO  (IOArray)
import           Control.Monad  (forM)
import           System.Random  (randomRIO)
import qualified Labyrinth.Tile as Tile
import           Labyrinth.Tile (Tile)

shuffle :: [a] -> IO [a]
shuffle xs = do
  ar <- newArray n xs
  forM [1..n] $ \i -> do
    j <- randomRIO (i, n)
    vi <- AIO.readArray ar i
    vj <- AIO.readArray ar j
    AIO.writeArray ar j vi
    return vj
  where
    n = length xs
    newArray :: Int -> [a] -> IO (IOArray Int a)
    newArray n' xs' = AIO.newListArray (1, n') xs'

rotateRandom :: Tile -> IO Tile
rotateRandom t  = do
  i <- randomRIO (0, 3)
  let r = foldl (.) id $ replicate i Tile.rotate
  return $ r t
