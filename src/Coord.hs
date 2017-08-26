module Coord where
import Data.Bifunctor (bimap)
import Board (coords)

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
left = Coord (rmv, stay)

right :: Move
right = Coord (add, stay)

down :: Move
down = Coord (stay, rmv)

up :: Move
up = Coord (stay, add)

add :: Int -> Int
add x = x + 1

-- https://stackoverflow.com/a/4553701/824779
rmv :: Int -> Int
rmv x = x - 1

stay :: Int -> Int
stay = id

-- better use a bifunctor, and (,) is already a bifunctor
original = (map (coords 3) [0..2])
toRight = map (bimap add stay) original
