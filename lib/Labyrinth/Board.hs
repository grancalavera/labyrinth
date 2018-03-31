module Labyrinth.Board
  ( toRows
  )
where

import qualified Data.Map.Strict               as Map
import           Data.Map.Strict                ( Map )
import           Data.Maybe                     ( fromMaybe )
import           Labyrinth.Position             ( Position )

toRows :: Map Position a -> [[(Position, a)]]
toRows m = map (Map.toList . (`getRow` m)) (rowSpread m)

getRow :: Int -> Map Position a -> Map Position a
getRow r = Map.filterWithKey (curry $ (== r) . rowIndex)

rowMin :: Map Position a -> Maybe Int
rowMin m = rowIndex . fst <$> Map.minViewWithKey m

rowMax :: Map Position a -> Maybe Int
rowMax m = rowIndex . fst <$> Map.maxViewWithKey m

rowIndex :: (Position, a) -> Int
rowIndex = fst . fst

rowSpread :: Map Position a -> [Int]
rowSpread m = fromMaybe [] $ do
  mn <- rowMin m
  mx <- rowMax m
  return [mn .. mx]
