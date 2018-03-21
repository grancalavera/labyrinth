{-# LANGUAGE TemplateHaskell #-}
module Labyrinth.Cell
    ( Cell (..)
    ) where

import Labyrinth.Players (Player)
import Labyrinth.Goal (Goal)
import Labyrinth.Tile (Tile)
import Labyrinth.Gate (Gate)

data Cell = C
  { _tile    :: Maybe Tile
  , _gate    :: Maybe Gate
  , _goal    :: Maybe Goal
  , _playerY :: Maybe Player
  , _playerR :: Maybe Player
  , _playerB :: Maybe Player
  , _playerG :: Maybe Player
  }
