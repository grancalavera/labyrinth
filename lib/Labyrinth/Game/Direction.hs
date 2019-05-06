module Labyrinth.Game.Direction
  ( Direction(..)
  , next
  , previous
  , random
  , opposite
  )
where

import qualified Data.List.NonEmpty            as NE
import           Control.Monad.Random.Strict              ( RandomGen
                                                          , Rand
                                                          )
import           Data.Random                              ( choose )

data Direction = North | West | South | East deriving (Show, Eq, Ord, Enum)

directions :: [Direction]
directions = enumFrom (toEnum 0)

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

random :: RandomGen g => Rand g Direction
random = choose $ NE.fromList directions
