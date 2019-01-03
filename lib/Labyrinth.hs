module Labyrinth
  ( Color(..)
  , Direction(..)
  , DGame(..)
  , DTile(..)
  , Game(..)
  , Gate(..)
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

import           Labyrinth.Game.Direction       ( Direction(..) )
import           Labyrinth.Game                as Game
import           Labyrinth.Game                 ( Game(..)
                                                , Phase(..)
                                                )
import           Labyrinth.Game.Description     ( DGame(..)
                                                , DTile(..)
                                                )
import           Labyrinth.Game.Gate            ( Gate(..) )
import qualified Labyrinth.Game.Treasure       as Treasure
import           Labyrinth.Game.Treasure        ( Treasure(..) )
import           Labyrinth.Game.Players         ( Players
                                                , Color(..)
                                                )
import           Labyrinth.Game.Position        ( Position )
import           Labyrinth.Game.Tile            ( Terrain(..)
                                                , Tile
                                                )

gameFromDescription :: DGame -> IO Game
gameFromDescription = Game.fromDescription

treasures :: [Treasure]
treasures = Treasure.treasures
