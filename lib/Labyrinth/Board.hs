module Labyrinth.Board
    ( Board
    , Position
    , fixedTiles
    , toList
    ) where

import qualified Data.Map         as Map
import           Data.Map         (Map)
import           Data.Monoid      ((<>))
import           Labyrinth.Tile   (Tile)
import qualified Labyrinth.Tile   as Tile

type Position = (Int, Int)
data Cell a = Cell Position a
data Board a = Board (Map Position a) deriving (Show)

instance Monoid (Board a) where
  mempty = Board mempty
  Board l `mappend` Board r = Board (Map.union r l)

toList :: Board a -> [(Position, a)]
toList (Board m) = Map.toList m

fromCell :: Cell a -> Board a
fromCell (Cell p x) = Board (Map.insert p x mempty)

fromCells :: [Cell a] -> Board a
fromCells cs = foldl (<>) mempty (map fromCell cs)

fixedTiles :: Board Tile
fixedTiles = fromCells
  [ Cell (2, 0) (Tile.mirror Tile.gate)
  , Cell (4, 0) (Tile.mirror Tile.gate)
  , Cell (6, 0) (Tile.mirror Tile.gate)
  , Cell (0, 2) (Tile.rotate' Tile.gate)
  , Cell (0, 4) (Tile.rotate' Tile.gate)
  , Cell (0, 6) (Tile.rotate' Tile.gate)
  , Cell (8, 2) (Tile.rotate Tile.gate)
  , Cell (8, 4) (Tile.rotate Tile.gate)
  , Cell (8, 6) (Tile.rotate Tile.gate)
  , Cell (2, 8) Tile.gate
  , Cell (4, 8) Tile.gate
  , Cell (6, 8) Tile.gate
  , Cell (1, 1) (Tile.mirror Tile.corner)
  , Cell (7, 1) (Tile.rotate Tile.corner)
  , Cell (1, 7) (Tile.rotate' Tile.corner)
  , Cell (7, 7) Tile.corner
  , Cell (3, 1) (Tile.mirror Tile.fork)
  , Cell (5, 1) (Tile.mirror Tile.fork)
  , Cell (1, 3) (Tile.rotate' Tile.fork)
  , Cell (1, 5) (Tile.rotate' Tile.fork)
  , Cell (7, 3) (Tile.rotate Tile.fork)
  , Cell (7, 5) (Tile.rotate Tile.fork)
  , Cell (3, 7) Tile.fork
  , Cell (5, 7) Tile.fork
  , Cell (3, 3) (Tile.rotate' Tile.fork)
  , Cell (5, 3) (Tile.mirror Tile.fork)
  , Cell (3, 5) Tile.fork
  , Cell (5, 5) (Tile.rotate Tile.fork)
  ]
