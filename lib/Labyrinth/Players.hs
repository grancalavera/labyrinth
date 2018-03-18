{-# LANGUAGE TemplateHaskell #-}

module Labyrinth.Players
    ( Player (..)
    , Color (..)
    , Players
    , color
    , name
    , colors
    , fromList
    , toList
    ) where

import Lens.Micro.TH (makeLenses)

data Color = Yellow | Red  | Blue | Green deriving (Show, Eq, Ord, Enum)

data Player = Player
  { _color :: Color
  , _name  :: String
  } deriving (Show, Eq)
makeLenses ''Player

colors :: [Color]
colors = [(toEnum 0)..]

data Players = P2 Player Player
             | P3 Player Player Player
             | P4 Player Player Player Player
             deriving (Eq, Show)

toList :: Players -> [Player]
toList (P2 p1 p2)       = [p1, p2]
toList (P3 p1 p2 p3)    = [p1, p2, p3]
toList (P4 p1 p2 p3 p4) = [p1, p2, p3, p4]

fromList :: [Player] -> Maybe Players
fromList (p1:p2:[])       = Just (P2 p1 p2)
fromList (p1:p2:p3:[])    = Just (P3 p1 p2 p3)
fromList (p1:p2:p3:p4:[]) = Just (P4 p1 p2 p3 p4)
fromList _                = Nothing
