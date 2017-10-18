{-# LANGUAGE TemplateHaskell #-}
module Labyrinth.Players
    ( Name
    , Players (..)
    , Player (..)
    , Color (..)
    , color
    , name
    , next
    , fromPlayer
    , lookup
    ) where

import Prelude hiding (lookup)
import Lens.Micro ((^.))
import Lens.Micro.TH (makeLenses)

import qualified Data.Map as M

type Name = String
data Color = Yellow | Blue | Green | Red deriving (Show, Eq, Ord)

data Players = Players (M.Map Color Player) deriving (Show, Eq)

instance Monoid Players where
  Players l `mappend` Players r = Players (M.union r l)
  mempty = Players M.empty

data Player = Player
    { _color :: Color
    , _name  :: Name
    } deriving (Show, Eq)
makeLenses ''Player

fromPlayer :: Player -> Players
fromPlayer p = Players (M.insert  (p ^. color) p mempty)

lookup :: Player -> Players -> Maybe Player
lookup p (Players ps) = M.lookup (p ^. color) ps

next :: Player -> Players -> Maybe Player
next =  undefined
-- next currP ps = case hasNext of
--   False -> Nothing
--   True  -> findNext (currP ^. color)
--   where
--     hasNext = M.size ps > 1
--     findNext c = case M.lookup (nextColor c) ps of
--       Just p  -> Just p
--       Nothing -> findNext (nextColor c)

nextColor :: Color -> Color
nextColor Yellow  = Blue
nextColor Blue    = Green
nextColor Green   = Red
nextColor Red     = Yellow
