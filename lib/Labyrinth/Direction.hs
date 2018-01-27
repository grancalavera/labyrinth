module Labyrinth.Direction
    ( Direction (..)
    , next
    , previous
    ) where

data Direction = North | West | South | East deriving (Show, Eq, Ord)

next :: Direction -> Direction
next North  = West
next West   = South
next South  = East
next East   = North

previous :: Direction -> Direction
previous = next . next . next
