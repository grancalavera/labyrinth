{-# LANGUAGE TemplateHaskell #-}
module Labyrinth.Game where

import           Lens.Micro         ((%~), (&), (^.), (.~))
import           Lens.Micro.TH      (makeLenses)

import           Labyrinth.Players  (Player(..), Players)
import qualified Labyrinth.Players  as Players

data Game = Game
    { _currentPlayer :: Maybe Player
    , _players :: Players
    } deriving (Show, Eq)
makeLenses ''Game

initial :: Game
initial = Game
    { _currentPlayer = Nothing
    , _players       = Players.initial
    }

addPlayer ::  Player -> Game -> Game
addPlayer p g = g & players %~ (Players.add p)

nextPlayer :: Game -> Maybe Game
nextPlayer g = do
  currentP  <- g ^. currentPlayer
  nextP     <- Players.next currentP (g ^. players)
  return (g & currentPlayer .~ (Just nextP))
