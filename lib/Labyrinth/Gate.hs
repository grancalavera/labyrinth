module Labyrinth.Gate
    ( Gate (..)
    , Open
    ) where

import qualified Data.Set as Set
import           Labyrinth.Direction (Direction(..))
import           Labyrinth.Transitable (Transitable, exits)

type Open = Bool
data Gate = Gate Direction Open deriving (Eq, Show)

instance Transitable Gate where
  exits d _ = Set.singleton d
