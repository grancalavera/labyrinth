{-# LANGUAGE TemplateHaskell #-}

module Labyrinth.Cell
    ( Cell
    , tile
    , fromTile
    ) where

import Lens.Micro     ((^.), (.~), (&))
import Lens.Micro.TH  (makeLenses)
import Labyrinth.Tile (Tile (..), Terrain (..), Direction (..))

data Cell = Cell
  { _tile :: Tile
  } deriving (Show, Eq)
makeLenses ''Cell

instance Monoid Cell where
  mempty = Cell
    { _tile = Tile Blank North
    }
  _ `mappend` r = Cell
    { _tile = r ^. tile
    }

fromTile :: Tile -> Cell
fromTile t = mempty & tile .~ t
