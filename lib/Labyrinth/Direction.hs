module Labyrinth.Direction
    ( Direction (..)
    , next
    , previous
    , random
    ) where

import System.Random (randomRIO)

data Direction = North | West | South | East deriving (Show, Eq, Ord, Enum)

next :: Direction -> Direction
next North  = West
next West   = South
next South  = East
next East   = North

previous :: Direction -> Direction
previous = next . next . next

random :: IO Direction
random = randomRIO (0,3) >>= return . toEnum
