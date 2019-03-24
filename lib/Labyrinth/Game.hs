module Labyrinth.Game
  ( Player(..)
  , Players
  , Color(..)
  , PlayOrder(..)
  , Game(..)
  , Configuration
  , Position
  , playing
  )
where

import           Labyrinth.Game.Class                     ( Game(..)
                                                          , playing
                                                          )
import           Labyrinth.Game.Configuration             ( Configuration )
import           Labyrinth.Game.Player                    ( Color(..)
                                                          , PlayOrder(..)
                                                          , Player(..)
                                                          , Players
                                                          )
import           Labyrinth.Game.Position                  ( Position )
