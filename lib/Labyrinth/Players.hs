module Labyrinth.Players where

import qualified Data.Map as M

type Name = String
type PlayerSet = M.Map Color (Maybe Player)
data Color = Yellow | Blue | Green | Red deriving (Show, Eq, Ord)

data Player = Player
    { _name  :: Name
    , _color :: Color
    } deriving (Show)

initial :: PlayerSet
initial = M.fromList
  [ (Yellow, Nothing)
  , (Blue, Nothing)
  , (Green, Nothing)
  , (Red, Nothing)
  ]
