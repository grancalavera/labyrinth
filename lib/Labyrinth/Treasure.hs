module Labyrinth.Treasure
  ( Treasure(..)
  , Goal
  , Found
  , treasures
  )
where

data Treasure = Bat |  Beetle | Book | Chandelier | Chest | Crown | Dragon
              | Emerald | Genie | Ghost | Goblin | Gold | Helmet | Keys
              | Lizzard | Map | Moth | Owl | Princess | Rat | Ring | Skull
              | Spider | Sword deriving (Eq, Show, Enum, Ord)

type Goal = Treasure
type Found = Treasure

treasures :: [Treasure]
treasures = [(toEnum 0) ..]
