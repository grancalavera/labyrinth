module Labyrinth.Game.Gate
  ( Gate(..)
  )
where
import           Labyrinth.Game.Direction       ( Direction )
data Gate = Gate Direction Bool deriving (Eq, Show)
