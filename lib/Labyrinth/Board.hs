module Labyrinth.Board
    ( Board
    , Cell(..)
    , fixedTiles
    ) where

import qualified Data.Map         as Map
import           Data.Map         (Map)
import           Data.Monoid      ((<>))
import           Labyrinth.Tile   (Tile, Terrain(..))
import qualified Labyrinth.Tile   as Tile

type Position = (Int, Int)
data Cell a = Cell Position a
data Board a = Board (Map Position a)

instance Monoid (Board a) where
  mempty = Board mempty
  Board l `mappend` Board r = Board (Map.union r l)

fromCell :: Cell a -> Board a
fromCell (Cell p x) = Board (Map.insert p x mempty)

fromCells :: [Cell a] -> Board a
fromCells cs = foldl (<>) mempty (map fromCell cs)

fixedTiles :: Board Tile
fixedTiles = fromCells
  [ Cell (0, 0) (Tile.fromTerrain Blank)
  , Cell (1, 0) (Tile.fromTerrain Blank)
  , Cell (2, 0) (Tile.fromTerrain Gate)
  , Cell (3, 0) (Tile.fromTerrain Blank)
  , Cell (4, 0) (Tile.fromTerrain Gate)
  , Cell (5, 0) (Tile.fromTerrain Blank)
  , Cell (6, 0) (Tile.fromTerrain Gate)
  , Cell (7, 0) (Tile.fromTerrain Blank)
  , Cell (8, 0) (Tile.fromTerrain Blank)

  , Cell (0, 1) (Tile.fromTerrain Blank)
  , Cell (1, 1) (Tile.fromTerrain Blank)
  , Cell (2, 1) (Tile.fromTerrain Blank)
  , Cell (3, 1) (Tile.fromTerrain Blank)
  , Cell (4, 1) (Tile.fromTerrain Blank)
  , Cell (5, 1) (Tile.fromTerrain Blank)
  , Cell (6, 1) (Tile.fromTerrain Blank)
  , Cell (7, 1) (Tile.fromTerrain Blank)
  , Cell (8, 1) (Tile.fromTerrain Blank)

  , Cell (0, 2) (Tile.fromTerrain Blank)
  , Cell (1, 2) (Tile.fromTerrain Blank)
  , Cell (2, 2) (Tile.fromTerrain Blank)
  , Cell (3, 2) (Tile.fromTerrain Blank)
  , Cell (4, 2) (Tile.fromTerrain Blank)
  , Cell (5, 2) (Tile.fromTerrain Blank)
  , Cell (6, 2) (Tile.fromTerrain Blank)
  , Cell (7, 2) (Tile.fromTerrain Blank)
  , Cell (8, 2) (Tile.fromTerrain Blank)

  , Cell (0, 3) (Tile.fromTerrain Blank)
  , Cell (1, 3) (Tile.fromTerrain Blank)
  , Cell (2, 3) (Tile.fromTerrain Blank)
  , Cell (3, 3) (Tile.fromTerrain Blank)
  , Cell (4, 3) (Tile.fromTerrain Blank)
  , Cell (5, 3) (Tile.fromTerrain Blank)
  , Cell (6, 3) (Tile.fromTerrain Blank)
  , Cell (7, 3) (Tile.fromTerrain Blank)
  , Cell (8, 3) (Tile.fromTerrain Blank)

  , Cell (0, 4) (Tile.fromTerrain Blank)
  , Cell (1, 4) (Tile.fromTerrain Blank)
  , Cell (2, 4) (Tile.fromTerrain Blank)
  , Cell (3, 4) (Tile.fromTerrain Blank)
  , Cell (4, 4) (Tile.fromTerrain Blank)
  , Cell (5, 4) (Tile.fromTerrain Blank)
  , Cell (6, 4) (Tile.fromTerrain Blank)
  , Cell (7, 4) (Tile.fromTerrain Blank)
  , Cell (8, 4) (Tile.fromTerrain Blank)

  , Cell (0, 5) (Tile.fromTerrain Blank)
  , Cell (1, 5) (Tile.fromTerrain Blank)
  , Cell (2, 5) (Tile.fromTerrain Blank)
  , Cell (3, 5) (Tile.fromTerrain Blank)
  , Cell (4, 5) (Tile.fromTerrain Blank)
  , Cell (5, 5) (Tile.fromTerrain Blank)
  , Cell (6, 5) (Tile.fromTerrain Blank)
  , Cell (7, 5) (Tile.fromTerrain Blank)
  , Cell (8, 5) (Tile.fromTerrain Blank)

  , Cell (0, 6) (Tile.fromTerrain Blank)
  , Cell (1, 6) (Tile.fromTerrain Blank)
  , Cell (2, 6) (Tile.fromTerrain Blank)
  , Cell (3, 6) (Tile.fromTerrain Blank)
  , Cell (4, 6) (Tile.fromTerrain Blank)
  , Cell (5, 6) (Tile.fromTerrain Blank)
  , Cell (6, 6) (Tile.fromTerrain Blank)
  , Cell (7, 6) (Tile.fromTerrain Blank)
  , Cell (8, 6) (Tile.fromTerrain Blank)

  , Cell (0, 7) (Tile.fromTerrain Blank)
  , Cell (1, 7) (Tile.fromTerrain Blank)
  , Cell (2, 7) (Tile.fromTerrain Blank)
  , Cell (3, 7) (Tile.fromTerrain Blank)
  , Cell (4, 7) (Tile.fromTerrain Blank)
  , Cell (5, 7) (Tile.fromTerrain Blank)
  , Cell (6, 7) (Tile.fromTerrain Blank)
  , Cell (7, 7) (Tile.fromTerrain Blank)
  , Cell (8, 7) (Tile.fromTerrain Blank)

  , Cell (0, 8) (Tile.fromTerrain Blank)
  , Cell (1, 8) (Tile.fromTerrain Blank)
  , Cell (2, 8) (Tile.fromTerrain Gate)
  , Cell (3, 8) (Tile.fromTerrain Blank)
  , Cell (4, 8) (Tile.fromTerrain Gate)
  , Cell (5, 8) (Tile.fromTerrain Blank)
  , Cell (6, 8) (Tile.fromTerrain Gate)
  , Cell (7, 8) (Tile.fromTerrain Blank)
  , Cell (8, 0) (Tile.fromTerrain Blank)
  ]
