{-# LANGUAGE TemplateHaskell #-}
module Labyrinth.Players
    ( Name
    , Players (..)
    , Player (Player)
    , Color (Yellow, Blue, Green, Red)
    , color
    , name
    , next
    , invert
    , fromPlayer
    , lookup
    , lookupByColor
    ) where

import           Prelude hiding (lookup)
import           Lens.Micro     ((^.))
import           Lens.Micro.TH  (makeLenses)
import           Data.Map       (Map)
import qualified Data.Map       as M

type Name = String
type PlayerStore = Map Color Player
data Color = Clear | Yellow | Blue | Green | Red deriving (Show, Eq, Ord)

data Player = PlayerRemoved | Player
    { _color :: Color
    , _name  :: Name
    } deriving (Show, Eq)
makeLenses ''Player

data Players = Players PlayerStore deriving (Show, Eq)

instance Monoid Color where
  _ `mappend` c = c
  mempty = Clear

instance Monoid Players where
  mempty = Players M.empty
  Players l `mappend` Players r = Players (mergePlayers l r)

invert :: Players -> Players
invert (Players ps) = Players (M.map (const PlayerRemoved) ps)

mergePlayers :: PlayerStore -> PlayerStore -> PlayerStore
mergePlayers = M.mergeWithKey merge id id
  where
    merge :: Color -> Player -> Player -> Maybe Player
    merge _ PlayerRemoved _             = Nothing
    merge _ _             PlayerRemoved = Nothing
    merge _ _             p             = Just p

fromPlayer :: Player -> Players
fromPlayer p = Players (M.insert  (p ^. color) p mempty)

lookup :: Player -> Players -> Maybe Player
lookup p (Players ps) = M.lookup (p ^. color) ps

lookupByColor :: Color -> Players -> Maybe Player
lookupByColor c (Players ps) = M.lookup c ps

next :: Player -> Players -> Maybe Player
next current ps@(Players psMap)
  | M.size psMap < 2 = Nothing
  | otherwise        = next' (current ^. color)
  where
    next' c = case (lookupByColor (nextColor c) ps) of
      Just p -> Just p
      _      -> next' (nextColor c)

nextColor :: Color -> Color
nextColor Yellow  = Blue
nextColor Blue    = Green
nextColor Green   = Red
nextColor Red     = Yellow
