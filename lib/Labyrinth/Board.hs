module Labyrinth.Board
    ( Board
    , Position
    , blank
    , toList
    , fromList
    , toListByRow
    , map
    , toMap
    ) where

import           Prelude hiding (map)
import qualified Data.Map       as Map
import           Data.Map       (Map)
import           Labyrinth.Cell (Cell(..))

type Position = (Int, Int)
data Board a = Board (Map Position (Cell a)) deriving (Show, Eq)

instance Monoid (Board a) where
  mempty = Board mempty
  Board l `mappend` Board r = Board (Map.union r l)

map :: (Cell a -> Cell b) -> Board a -> Board b
map f (Board m) = Board (Map.map f m)

toMap :: Board a -> Map Position (Cell a)
toMap (Board m) = m

blank :: Board a
blank = fromList [((x, y), mempty) | x <- size, y <- size]
  where
    size :: [Int]
    size = [0..8]

toList :: Board a -> [(Position, Cell a)]
toList (Board m) = Map.toList m

toListByRow :: Int -> Board a -> [(Position, Cell a)]
toListByRow r = toList . (filterByRow r)

fromList :: [(Position, Cell a)] -> Board a
fromList ls = Board (Map.fromList ls)

filterByRow :: Int -> Board a -> Board a
filterByRow r (Board m) = Board (Map.filterWithKey byRow m)
  where
    byRow :: Position -> Cell a -> Bool
    byRow (_, i) _ = r == i
