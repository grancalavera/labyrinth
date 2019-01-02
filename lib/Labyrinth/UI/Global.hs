module Labyrinth.UI.Global
  ( Global(..)
  , Modal(..)
  , initial
  , screenIsBlocked
  , chooseCursor
  )
where

import           Brick
import           Labyrinth.UI.Internal

data Modal e = Modal

newtype Global e = Global (Maybe (Modal e))

initial :: Global e
initial = Global Nothing

screenIsBlocked :: Global e -> Bool
screenIsBlocked (Global Nothing) = False
screenIsBlocked _                = True

chooseCursor
  :: Global e -> Maybe ([CursorLocation Name] -> Maybe (CursorLocation Name))
chooseCursor _ = Nothing
