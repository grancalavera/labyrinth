module Point where
import Data.Bifunctor (bimap)
import Board (coords)

type Point = (Int,Int)

left :: Point -> Point
left = bimap rmv stay

right :: Point -> Point
right = bimap add stay

up :: Point -> Point
up = bimap stay rmv

down :: Point -> Point


-- left = bimap rmv stay
-- left :: Move
-- left = Point (rmv, stay)

-- right :: Move
-- right = Point (add, stay)

-- down :: Move
-- down = Point (stay, rmv)

-- up :: Move
-- up = Point (stay, add)

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
