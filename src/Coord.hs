module Coord where

data Coord a = Coord (a, a) deriving (Eq, Show)
type Move = Coord (Int -> Int)
type Point = Coord Int

instance Functor Coord where
  fmap f (Coord (x, y)) = Coord ((f x), (f y))

instance Applicative Coord where
  pure x = Coord (x, x)
  Coord (f,g) <*> Coord (x, y) = Coord ((f x), (g y))

makePoint :: Int -> Int -> Point
makePoint x y = Coord (x,y)

left :: Move
left = Coord (minus1, id)

right :: Move
right = Coord ((+1), id)

down :: Move
down  = Coord (id, minus1)

up :: Move
up    = Coord (id, (+1))

minus1 = \x -> x - 1

-- instance Show Coord where
--   show (Coord x y) = "(" ++ show x ++ "," ++ show y ++ ")"

