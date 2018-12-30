module Labyrinth.UI.Internal
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
