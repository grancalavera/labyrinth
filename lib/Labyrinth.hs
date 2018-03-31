module Labyrinth
  ( shuffle
  , randomElem
  , toRows
  , splitEvery
  , getRow
  , getCol
  , Position
  )
where

import qualified Data.Array.IO                 as AIO
import           Data.Array.IO                  ( IOArray )
import           Control.Monad                  ( forM )
import           System.Random                  ( randomRIO )
import qualified Data.Map.Strict               as Map
import           Data.Map.Strict                ( Map )
import           Data.Maybe                     ( fromMaybe )

type Row      = Int
type Col      = Int
type Position = (Row, Col)

randomElem :: [a] -> IO (Maybe a)
randomElem [] = return Nothing
randomElem xs = Just . (xs !!) <$> randomRIO (0, length xs - 1)

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

toRows :: Map Position a -> [[(Position, a)]]
toRows m = map (Map.toList . (`getRow` m)) (rowSpread m)

getRow :: Int -> Map Position a -> Map Position a
getRow r = Map.filterWithKey (curry $ (== r) . rowIndex)

getCol :: Int -> Map Position a -> Map Position a
getCol c = Map.filterWithKey (curry $ (== c) . colIndex)

rowMin :: Map Position a -> Maybe Int
rowMin m = rowIndex . fst <$> Map.minViewWithKey m

rowMax :: Map Position a -> Maybe Int
rowMax m = rowIndex . fst <$> Map.maxViewWithKey m

rowIndex :: (Position, a) -> Int
rowIndex = fst . fst

colIndex :: (Position, a) -> Int
colIndex = snd . fst

rowSpread :: Map Position a -> [Int]
rowSpread m = fromMaybe [] $ do
  mn <- rowMin m
  mx <- rowMax m
  return [mn .. mx]

splitEvery :: Int -> [a] -> [[a]]
splitEvery _ []   = []
splitEvery n list = first : splitEvery n rest
  where (first, rest) = splitAt n list

