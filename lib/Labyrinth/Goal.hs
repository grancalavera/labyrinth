module Labyrinth.Goal (
    Goal (..)
  , Found
  , Treasure (..)
  ) where

data Treasure = Bat |  Beetle | Book | Chandelier | Chest | Crown | Dragon
              | Emerald | Genie | Ghost | Goblin | Gold | Helmet | Keys
              | Lizzard | Map | Moth | Owl | Princess | Rat | Ring | Skull
              | Spider | Sword deriving (Eq, Show)

type Found = Bool
data Goal = Goal Treasure Found deriving (Eq, Show)
