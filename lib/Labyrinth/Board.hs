module Labyrinth.Board
    ( Board
    , Position
    , fromList
    ) where

import           Data.Monoid ((<>))
import qualified Data.Map       as Map
import           Data.Map       (Map)
import           Labyrinth      (Position)

data Board a = Board (Map Position a) deriving (Show, Eq)
instance Monoid (Board a) where
  mempty = Board mempty
  Board l `mappend` Board r = Board (l <> r)

fromList :: [(Position, a)] -> Board a
fromList ls = Board (Map.fromList ls)
