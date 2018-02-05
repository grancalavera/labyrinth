{-# LANGUAGE TemplateHaskell #-}

module Labyrinth.Players
    ( Name
    , Players (..)
    , Player (..)
    , Color (..)
    , color
    , colors
    , name
    , next
    , fromPlayer
    , lookup
    , toList
    ) where

import           Prelude hiding (lookup)
import           Lens.Micro     ((^.))
import           Lens.Micro.TH  (makeLenses)
import           Data.Map       (Map)
import qualified Data.Map       as Map

type Name = String
data Color = Yellow | Blue | Green | Red deriving (Show, Eq, Ord, Enum)

data Players = Players (Map Color Player) deriving (Show, Eq)

instance Monoid Players where
  Players l `mappend` Players r = Players (l `mappend` r)
  mempty = Players mempty

data Player = Player
    { _color :: Color
    , _name  :: Name
    } deriving (Show, Eq)
makeLenses ''Player

fromPlayer :: Player -> Players
fromPlayer p = Players (Map.insert  (p ^. color) p mempty)

-- lookup :: Player -> Players -> Maybe Player
-- lookup p (Players ps) = Map.lookup (p ^. color) ps

lookup :: Color -> Players -> Maybe Player
lookup c (Players ps) = Map.lookup c ps

toList :: Players -> [(Color, Player)]
toList (Players m) = Map.toList m

next :: Player -> Players -> Maybe Player
next current ps@(Players psMap)
  | Map.size psMap < 2 = Nothing
  | otherwise        = next' (current ^. color)
  where
    next' c = case (lookup (nextColor c) ps) of
      Just p -> Just p
      _      -> next' (nextColor c)

nextColor :: Color -> Color
nextColor c = toEnum $ ((1 + fromEnum c) `mod` (length colors))

colors :: [Color]
colors = [(toEnum 0::Color) ..]
