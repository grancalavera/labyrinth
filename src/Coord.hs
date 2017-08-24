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
left = Coord (rmv, stay)

right :: Move
right = Coord (add, stay)

down :: Move
down  = Coord (stay, rmv)

up :: Move
up    = Coord (stay, add)

add :: Int -> Int
add x = x + 1

rmv :: Int -> Int
rmv x = x - 1

stay :: Int -> Int
stay = id

-- instance Show Coord where
--   show (Coord x y) = "(" ++ show x ++ "," ++ show y ++ ")"

