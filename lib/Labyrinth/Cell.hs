{-# LANGUAGE TemplateHaskell #-}

module Labyrinth.Cell
    ( Cell(..)
    , rotate
    , rotate'
    ) where

import           Labyrinth.Direction (Direction(..))
import qualified Labyrinth.Direction as Direction

data Cell a = Empty | Cell Direction a deriving (Show, Eq)

rotate :: Cell a -> Cell a
rotate = rotateWith Direction.next

rotate' :: Cell a -> Cell a
rotate' = rotateWith Direction.previous

rotateWith :: (Direction -> Direction) -> Cell a -> Cell a
rotateWith _ Empty      = Empty
rotateWith f (Cell d x) = Cell (f d) x
