module Labyrinth.Random
where

import qualified Data.Array.IO                 as AIO
import           Data.Array.IO                  ( IOArray )
import           Control.Monad                  ( forM )
import           System.Random                  ( randomRIO )

choose :: [a] -> IO (Maybe a)
choose [] = return Nothing
choose xs = Just . (xs !!) <$> randomRIO (0, length xs - 1)

shuffle :: [a] -> IO [a]
shuffle xs = do
  ar <- newArray n xs
  forM [1 .. n] $ \i -> do
    j  <- randomRIO (i, n)
    vi <- AIO.readArray ar i
    vj <- AIO.readArray ar j
    AIO.writeArray ar j vi
    return vj
 where
  n = length xs
  newArray :: Int -> [a] -> IO (IOArray Int a)
  newArray n' = AIO.newListArray (1, n')
