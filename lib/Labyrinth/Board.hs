module Labyrinth.Board
    ( Board
    , Position
    , fromList
    , toList
    , filterByRow
    , toRows
    , map
    , filter
    , filterByPositions
    ) where

import           Prelude        hiding (map, filter)
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
toRows b = Prel.map (toList . (`filterByRow` b)) (rowSpread b)

filter :: (a -> Bool) -> Board a -> Board a
filter f (Board m) = Board $ Map.filter f m

filterByRow :: Int -> Board a -> Board a
filterByRow r (Board m) = Board $ Map.filterWithKey byRow m
  where
    byRow :: Position -> a -> Bool
    byRow (_, i) _ = i == r

filterByPositions :: [Position] -> Board a -> Board a
filterByPositions ps (Board m) = Board $ Map.filterWithKey byPositions m
  where
    byPositions :: Position -> a -> Bool
    byPositions p _ = p `elem` ps

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

