module Labyrinth.UI.Screen.Game
  ( GameS
  , initial
  )
where

import           Labyrinth.Game                 ( Game )

data GameS = GameS
  { _game :: Game
  } deriving (Show)

initial :: Game -> GameS
initial g = GameS { _game = g }
