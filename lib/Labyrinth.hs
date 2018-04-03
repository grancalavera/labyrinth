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
  , gameFromDescription
  , treasures
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
import qualified Labyrinth.Goal                as Goal
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

gameFromDescription :: DGame -> IO Game
gameFromDescription = Game.fromDescription

treasures :: [Treasure]
treasures = Goal.treasures
