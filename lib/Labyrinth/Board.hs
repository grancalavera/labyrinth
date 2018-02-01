module Labyrinth.Board
    ( Board
    , Position
    , fromList
    , toList
    , filterByRow
    , toRows
    , map
    , mapWithKey
    , filter
    , filterByPositions
    , lookup
    , insert
    ) where

import           Prelude        hiding (map, filter, lookup)
import qualified Prelude        as Prel
import qualified Data.Map       as Map
import           Data.Map       (Map)
import           Data.Maybe     (fromMaybe)
import           Labyrinth      (Position)

data Board a = Board (Map Position a) deriving (Show, Eq)
instance Monoid (Board a) where
  mempty = Board mempty
  Board l `mappend` Board r = Board (l `mappend` r)

fromList :: [(Position, a)] -> Board a
fromList ls = Board (Map.fromList ls)

toList :: Board a -> [(Position, a)]
toList (Board m) = Map.toList m

map :: (a -> b) -> Board a -> Board b
map f (Board m) = Board $ Map.map f m

mapWithKey :: (Position -> a -> b) -> Board a -> Board b
mapWithKey f (Board m) = Board $ Map.mapWithKey f m

toRows :: Board a -> [[(Position, a)]]
toRows b = Prel.map (toList . (`filterByRow` b)) (rowSpread b)

filter :: (a -> Bool) -> Board a -> Board a
filter f (Board m) = Board $ Map.filter f m

insert :: Position -> a -> Board a -> Board a
insert k x (Board m) = Board $ Map.insert k x m

-- i need lookup to place players in the initial corners
lookup :: Position -> (Board a) -> Maybe a
lookup p (Board m) = Map.lookup p m

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
