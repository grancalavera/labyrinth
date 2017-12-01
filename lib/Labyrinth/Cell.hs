{-# LANGUAGE TemplateHaskell #-}

module Labyrinth.Cell
    ( Cell
    , tile
    , fromTile
    ) where

import Lens.Micro.TH  (makeLenses)
import Labyrinth.Tile (Tile)

data Cell = Cell
  { _tile :: Tile
  } deriving (Show, Eq)
makeLenses ''Cell

fromTile :: Tile -> Cell
fromTile = Cell
