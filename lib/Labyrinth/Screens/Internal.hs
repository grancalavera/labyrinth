module Labyrinth.Screens.Internal
  ( ResourceName(..)
  )
where

data ResourceName
    = NameField
    | YellowField
    | RedField
    | BlueField
    | GreenField
    | AnyResource deriving (Eq, Ord, Show)
