module Labyrinth.Game
  ( Player(..)
  , Players
  , Color(..)
  , PlayOrder(..)
  , Game(..)
  , Configuration
  )
where

import           Lens.Micro.TH                  ( makeLenses )
import           Labyrinth.Game.Position        ( Position )
import           Labyrinth.Game.Board           ( Board )
import           Labyrinth.Game.Cell            ( TileCell
                                                , GateCell
                                                )
import           Labyrinth.Game.Treasure        ( TreasureMap )
import           Data.Map.Strict                ( Map )
import           Labyrinth.Game.Player          ( Player(..)
                                                , Players
                                                , Color(..)
                                                , PlayOrder(..)
                                                )
import           Labyrinth.Game.Configuration   ( Configuration )
import           Labyrinth.Game.Class   ( Game(..) )
