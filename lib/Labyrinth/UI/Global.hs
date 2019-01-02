module Labyrinth.UI.Global
  ( Global(..)
  , Modal(..)
  , initial
  , screenIsBlocked
  )
where

data Modal e = Modal

newtype Global e = Global (Maybe (Modal e))

initial :: Global e
initial = Global Nothing

screenIsBlocked :: Global e -> Bool
screenIsBlocked (Global Nothing) = False
screenIsBlocked _                = True
