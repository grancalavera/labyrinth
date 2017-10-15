{-# LANGUAGE TemplateHaskell #-}
module Labyrinth.Game where

import           Lens.Micro         ((%~), (&))
import           Lens.Micro.TH      (makeLenses)

import           Labyrinth.Players  (Player(..), Players)
import qualified Labyrinth.Players  as Players

data Game = Game
    { _currentPlayer :: Maybe Player
    , _players :: Players
    } deriving (Show)
makeLenses ''Game

initial :: Game
initial = Game
    { _currentPlayer = Nothing
    , _players       = Players.initial
    }

addPlayer :: Game -> Player -> Game
addPlayer g p = g & players %~ (Players.add p)
