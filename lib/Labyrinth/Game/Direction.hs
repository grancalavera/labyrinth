module Labyrinth.Game.Direction
  ( Direction(..)
  , next
  , previous
  , random
  , opposite
  )
where

import           System.Random                  ( randomRIO )

data Direction = North | West | South | East deriving (Show, Eq, Ord, Enum)

next :: Direction -> Direction
next North = West
next West  = South
next South = East
next East  = North

opposite :: Direction -> Direction
opposite North = South
opposite South = North
opposite West  = East
opposite East  = West

previous :: Direction -> Direction
previous = next . next . next

random :: IO Direction
random = toEnum <$> randomRIO (0, 3)
