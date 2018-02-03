module Labyrinth
  ( shuffle
  , halve
  , filterByPositions
  , toRows
  , Position
  ) where

import qualified Data.Array.IO  as AIO
import           Data.Array.IO  (IOArray)
import           Control.Monad  (forM)
import           System.Random  (randomRIO)
import qualified Data.Map       as Map
import           Data.Map       (Map)
import           Data.Maybe     (fromMaybe)

type Position = (Int, Int)

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

halve :: [a] -> ([a],[a])
halve [] = ([], [])
halve ls = (take half ls, drop half ls)
  where
    half :: Int
    half = length ls `div` 2

filterByPositions :: [Position] -> Map Position a -> Map Position a
filterByPositions ps m = Map.filterWithKey byPositions m
  where
    byPositions :: Position -> a -> Bool
    byPositions p _ = p `elem` ps

toRows :: Map Position a -> [[(Position, a)]]
toRows m = map (Map.toList . (`filterByRow` m)) (rowSpread m)

filterByRow :: Int -> Map Position a -> Map Position a
filterByRow r m = Map.filterWithKey byRow m
  where
    byRow :: Position -> a -> Bool
    byRow (_, i) _ = i == r

rowMin :: Map Position a -> Maybe Int
rowMin m  = do
  (((_, i), _), _) <- Map.minViewWithKey m
  return i

rowMax :: Map Position a -> Maybe Int
rowMax m  = do
  (((_, i), _), _) <- Map.maxViewWithKey m
  return i

rowSpread :: Map Position a -> [Int]
rowSpread m = fromMaybe [] $ do
  mn <- rowMin m
  mx <- rowMax m
  return [mn..mx]
