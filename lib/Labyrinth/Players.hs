{-# LANGUAGE TemplateHaskell #-}
module Labyrinth.Players where

import Lens.Micro ((^.))
import Lens.Micro.TH (makeLenses)

import qualified Data.Map as M

type Name = String
type Players = M.Map Color Player
data Color = Yellow | Blue | Green | Red deriving (Show, Eq, Ord)

data Player = Player
    { _color :: Color
    , _name  :: Name
    } deriving (Show, Eq)
makeLenses ''Player

initial :: Players
initial = M.empty

add :: Players -> Player -> Players
add ps p = M.insert (p ^. color) p ps

addFirst :: Player -> Players
addFirst = add initial

next :: Players -> Player -> Maybe Player
next ps currentP = if hasNext
    then findNext (currentP ^. color)
    else Nothing
  where
    hasNext = M.size ps > 1
    findNext c = case M.lookup (nextColor c) ps of
      Just p  -> Just p
      Nothing -> findNext (nextColor c)

nextColor :: Color -> Color
nextColor Yellow  = Blue
nextColor Blue    = Green
nextColor Green   = Red
nextColor Red     = Yellow
