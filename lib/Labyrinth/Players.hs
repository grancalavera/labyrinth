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
    , toMap
    , next
    ) where

import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Data.Maybe      (fromJust)
import qualified Data.List       as L
import qualified Data.Text       as T
import           Lens.Micro      ((^.))
import           Lens.Micro.TH   (makeLenses)

data Color = Yellow | Red  | Blue | Green deriving (Show, Eq, Ord, Enum)

data Player = Player
  { _color :: Color
  , _name  :: T.Text
  } deriving (Show, Eq)
makeLenses ''Player

instance Ord Player where
  l <= r = l ^. color <= r ^. color

colors :: [Color]
colors = [(toEnum 0)..]

data Players = P2 Player Player
             | P3 Player Player Player
             | P4 Player Player Player Player
             deriving (Eq, Show)

toList :: Players -> [Player]
toList ps = L.sort $ case ps of
  (P2 p1 p2)       -> [p1, p2]
  (P3 p1 p2 p3)    -> [p1, p2, p3]
  (P4 p1 p2 p3 p4) -> [p1, p2, p3, p4]

fromList :: [Player] -> Maybe Players
fromList (p1:p2:[])       = Just (P2 p1 p2)
fromList (p1:p2:p3:[])    = Just (P3 p1 p2 p3)
fromList (p1:p2:p3:p4:[]) = Just (P4 p1 p2 p3 p4)
fromList _                = Nothing

toMap :: Players -> Map Color Player
toMap = Map.fromList . (map (\p -> (p ^. color, p))) . toList

next :: Player -> Players -> Player
next p ps = (cycle l) !! (i + 1)
  where
    i = fromJust $ L.elemIndex p l
    l = toList ps
