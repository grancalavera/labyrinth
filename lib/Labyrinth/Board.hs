module Labyrinth.Board
    ( Board
    , Position
    , toList
    , fromList
    ) where

import qualified Data.Map         as Map
import           Data.Map         (Map)
import           Data.Monoid      ((<>))

type Position = (Int, Int)
data Board a = Board (Map Position a) deriving (Show, Eq)

instance Monoid (Board a) where
  mempty = Board mempty
  Board l `mappend` Board r = Board (Map.union r l)

toList :: Board a -> [(Position, a)]
toList (Board m) = Map.toList m

fromList :: [(Position, a)] -> Board a
fromList ls = Board (Map.fromList ls)
