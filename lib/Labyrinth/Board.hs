module Labyrinth.Board
    ( Board
    , Position

    -- temp
    , Cell (..)
    , fromCells
    , toList
    -- temp

    ) where

import           Control.Monad    (forM)
import qualified Data.Map         as Map
import           Data.Map         (Map)
import           Data.Monoid      ((<>))

type Position = (Int, Int)
data Cell a = Cell Position a
data Board a = Board (Map Position a) deriving (Show, Eq)

instance Monoid (Board a) where
  mempty = Board mempty
  Board l `mappend` Board r = Board (Map.union r l)

toList :: Board a -> [(Position, a)]
toList (Board m) = Map.toList m

fromCell :: Cell a -> Board a
fromCell (Cell p x) = Board (Map.insert p x mempty)

fromCells :: [Cell a] -> Board a
fromCells cs = foldl (<>) mempty (map fromCell cs)

