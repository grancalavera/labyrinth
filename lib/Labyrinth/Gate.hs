module Labyrinth.Gate
    ( Gate (..)
    , Open
    , exits
    , open
    , closed
    ) where

import qualified Data.Set as Set
import           Labyrinth.Transitable (Transitable, exits)

type Open = Bool
data Gate = Gate Open deriving (Eq, Show)

open :: Gate
open = Gate True

closed :: Gate
closed = Gate False

instance Transitable Gate where
  exits _ = Set.singleton
