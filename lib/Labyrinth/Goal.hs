module Labyrinth.Goal (
    Goal (..)
  , Found
  , Treasure (..)
  , treasures
  , fromTreasure
  ) where

data Treasure = Bat |  Beetle | Book | Chandelier | Chest | Crown | Dragon
              | Emerald | Genie | Ghost | Goblin | Gold | Helmet | Keys
              | Lizzard | Map | Moth | Owl | Princess | Rat | Ring | Skull
              | Spider | Sword deriving (Eq, Show, Enum, Ord)

type Found = Bool
data Goal = Goal Treasure Found deriving (Eq, Show)

fromTreasure :: Treasure -> Goal
fromTreasure t = Goal t False

treasures :: [Treasure]
treasures = [Bat ..]
