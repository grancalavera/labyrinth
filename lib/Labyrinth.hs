module Labyrinth
  ( Color(..)
  , Direction(..)
  , DGame(..)
  , DTile(..)
  , Game(..)
  , Gate(..)
  , Goal(..)
  , Phase(..)
  , Players
  , Position
  , Terrain(..)
  , Tile
  , Treasure(..)
  , makeGame
  )
where

import           Labyrinth.Direction            ( Direction(..) )
import           Labyrinth.Game                as Game
import           Labyrinth.Game                 ( Game(..)
                                                , Phase(..)
                                                )
import           Labyrinth.Game.Description     ( DGame(..)
                                                , DTile(..)
                                                )
import           Labyrinth.Gate                 ( Gate(..) )
import           Labyrinth.Goal                 ( Goal(..)
                                                , Treasure(..)
                                                )
import           Labyrinth.Players              ( Players
                                                , Color(..)
                                                )
import           Labyrinth.Position             ( Position )
import           Labyrinth.Tile                 ( Terrain(..)
                                                , Tile
                                                )

makeGame :: DGame -> IO Game
makeGame = Game.make
