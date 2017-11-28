module Labyrinth.Board
    ( Board
    , Position
    , fixedTiles
    , toList
    ) where

import qualified Data.Map         as Map
import           Data.Map         (Map)
import           Data.Monoid      ((<>))
import           Labyrinth.Tile   (Tile(..), Terrain(..), Direction(..))

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
  [ Cell (2, 0) (Tile Gate South)
  , Cell (4, 0) (Tile Gate South)
  , Cell (6, 0) (Tile Gate South)
  , Cell (0, 2) (Tile Gate East)
  , Cell (0, 4) (Tile Gate East)
  , Cell (0, 6) (Tile Gate East)
  , Cell (8, 2) (Tile Gate West)
  , Cell (8, 4) (Tile Gate West)
  , Cell (8, 6) (Tile Gate West)
  , Cell (2, 8) (Tile Gate North)
  , Cell (4, 8) (Tile Gate North)
  , Cell (6, 8) (Tile Gate North)
  , Cell (1, 1) (Tile Corner South)
  , Cell (7, 1) (Tile Corner West)
  , Cell (1, 7) (Tile Corner East)
  , Cell (7, 7) (Tile Corner North)
  , Cell (3, 1) (Tile Fork South)
  , Cell (5, 1) (Tile Fork South)
  , Cell (1, 3) (Tile Fork East)
  , Cell (1, 5) (Tile Fork East)
  , Cell (7, 3) (Tile Fork West)
  , Cell (7, 5) (Tile Fork West)
  , Cell (3, 7) (Tile Fork North)
  , Cell (5, 7) (Tile Fork North)
  , Cell (3, 3) (Tile Fork East)
  , Cell (5, 3) (Tile Fork South)
  , Cell (3, 5) (Tile Fork North)
  , Cell (5, 5) (Tile Fork West)
  ]
