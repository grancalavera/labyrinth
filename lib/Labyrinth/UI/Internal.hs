module Labyrinth.UI.Internal
  ( Name(..)
  )
where

data Name
    = NameField
    | YellowField
    | RedField
    | BlueField
    | GreenField
    | AnyResource deriving (Eq, Ord, Show)
