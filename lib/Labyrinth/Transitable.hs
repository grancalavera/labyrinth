module Labyrinth.Transitable
    ( Transitable
    , exits
    ) where

import Labyrinth.Direction (Direction)
import Data.Set (Set)

class Transitable a where
  exits :: a -> Direction -> Set Direction
