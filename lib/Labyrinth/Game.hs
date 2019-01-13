module Labyrinth.Game
  ( Player(..)
  , Players
  , Color(..)
  , PlayOrder(..)
  , Game(..)
  , Configuration
  , players
  , playing
  )
where

import           Lens.Micro.TH                  ( makeLenses )
import           Labyrinth.Game.Player          ( Player(..)
                                                , Players
                                                , Color(..)
                                                , PlayOrder(..)
                                                )
import           Labyrinth.Game.Configuration   ( Configuration )

data Game = Game
  { _players :: Players
  , _playing :: Player
  } deriving (Show)

makeLenses  ''Game
