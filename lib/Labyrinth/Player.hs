{-# LANGUAGE TemplateHaskell #-}
module Labyrinth.Player
    ( Player (..)
    , Color (..)
    , color
    , name
    , colors
    ) where

import Lens.Micro.TH (makeLenses)

data Color = Yellow | Blue | Green | Red deriving (Show, Eq, Ord, Enum)

data Player = Player
  { _color :: Color
  , _name  :: String
  } deriving (Show, Eq)
makeLenses ''Player

colors :: [Color]
colors = [(toEnum 0::Color) ..]
