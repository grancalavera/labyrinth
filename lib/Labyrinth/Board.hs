module Labyrinth.Board
    ( Board
    , Position
    , fromList
    , filterByRow
    , toRows
    , map
    ) where

import           Prelude        hiding (map)
import qualified Prelude        as Prel
import           Data.Monoid    ((<>))
import qualified Data.Map       as Map
import           Data.Map       (Map)
import           Data.Maybe     (fromMaybe)
import           Labyrinth      (Position)

data Board a = Board (Map Position a) deriving (Show, Eq)
instance Monoid (Board a) where
  mempty = Board mempty
  Board l `mappend` Board r = Board (l <> r)

fromList :: [(Position, a)] -> Board a
fromList ls = Board (Map.fromList ls)

toList :: Board a -> [(Position, a)]
toList (Board m) = Map.toList m

map :: (a -> b) -> Board a -> Board b
map f (Board m) = Board $ Map.map f m

toRows :: Board a -> [[(Position, a)]]
toRows b = Prel.map (toList . (filterByRow b)) (rowSpread b)

filterByRow :: Board a -> Int -> Board a
filterByRow (Board m) r = Board $ Map.filterWithKey (\(_, i) -> \_ -> r == i) m

rowMin :: Board a -> Maybe Int
rowMin (Board b) = do
  (((_, i), _), _) <- Map.minViewWithKey b
  return i

rowMax :: Board a -> Maybe Int
rowMax (Board b) = do
  (((_, i), _), _) <- Map.maxViewWithKey b
  return i

rowSpread :: Board a -> [Int]
rowSpread b = fromMaybe [] $ do
  mn <- rowMin b
  mx <- rowMax b
  return [mn..mx]

