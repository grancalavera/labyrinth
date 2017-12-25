{-# LANGUAGE TemplateHaskell #-}

module Labyrinth.Cell
    ( Cell(..)
    , rotate
    , rotate'
    , randomRotate
    ) where

import           System.Random       (randomRIO)
import           Labyrinth.Direction (Direction(..))
import qualified Labyrinth.Direction as Direction

data Cell a = Empty | Cell Direction a deriving (Show, Eq)

instance Monoid (Cell a) where
  mempty = Empty
  Empty `mappend` c = c
  c `mappend` Empty = c
  _ `mappend` c = c

rotate :: Cell a -> Cell a
rotate = rotateWith Direction.next

rotate' :: Cell a -> Cell a
rotate' = rotateWith Direction.previous

randomRotate :: Cell a -> IO (Cell a)
randomRotate c  = do
  i <- randomRIO (0, 3)
  let r = foldl (.) id $ replicate i rotate
  return $ r c

rotateWith :: (Direction -> Direction) -> Cell a -> Cell a
rotateWith _ Empty      = Empty
rotateWith f (Cell d x) = Cell (f d) x
