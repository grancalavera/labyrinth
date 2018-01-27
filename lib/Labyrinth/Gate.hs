module Labyrinth.Gate
    ( Gate (..)
    ) where
import Labyrinth.Direction (Direction)
data Gate = Gate Direction Bool deriving (Eq, Show)
