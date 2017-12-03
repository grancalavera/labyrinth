module Labyrinth.Board
    ( Board
    , Position
    , toList
    , fromList
    , toListByRow
    ) where

import qualified Data.Map       as Map
import           Data.Map       (Map)
import           Labyrinth.Cell (Cell)

type Position = (Int, Int)
data Board = Board (Map Position Cell) deriving (Show, Eq)

instance Monoid Board where
  mempty = Board mempty
  Board l `mappend` Board r = Board (Map.union r l)

toList :: Board -> [(Position, Cell)]
toList (Board m) = Map.toList m

toListByRow :: Int -> Board -> [(Position, Cell)]
toListByRow r = toList . (filterByRow r)

fromList :: [(Position, Cell)] -> Board
fromList ls = Board (Map.fromList ls)

filterByRow :: Int -> Board -> Board
filterByRow r (Board m) = Board (Map.filterWithKey byRow m)
  where
    byRow :: Position -> Cell -> Bool
    byRow (_, i) _ = r == i
