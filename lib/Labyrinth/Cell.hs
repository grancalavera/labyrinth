{-# LANGUAGE TemplateHaskell #-}
module Labyrinth.Cell
    ( Cell (..)
    ) where

import Labyrinth.Players (Player)
import Labyrinth.Goal (Goal)
import Labyrinth.Tile (Tile)

data Cell = C
  { _tile    :: Tile
  , _goal    :: Maybe Goal
  , _playerY :: Maybe Player
  , _playerR :: Maybe Player
  , _playerB :: Maybe Player
  , _playerG :: Maybe Player
  }
