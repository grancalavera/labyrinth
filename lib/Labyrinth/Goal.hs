module Labyrinth.Goal (
    Goal (..)
  , Found
  , Treasure (..)
  , treasures
  ) where

data Treasure = Bat |  Beetle | Book | Chandelier | Chest | Crown | Dragon
              | Emerald | Genie | Ghost | Goblin | Gold | Helmet | Keys
              | Lizzard | Map | Moth | Owl | Princess | Rat | Ring | Skull
              | Spider | Sword deriving (Eq, Show, Enum, Ord)

type Found = Bool
data Goal = Goal Treasure Found deriving (Eq, Show)

treasures :: [Treasure]
treasures = [(toEnum 0) ..]
