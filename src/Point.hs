module Point where
import Data.Bifunctor (bimap)

type Point = (Int,Int)

left :: Point -> Point
left = bimap rmv stay

right :: Point -> Point
right = bimap add stay

up :: Point -> Point
up = bimap stay rmv

down :: Point -> Point
down = bimap stay add

add :: Int -> Int
add x = x + 1

-- https://stackoverflow.com/a/4553701/824779
rmv :: Int -> Int
rmv x = x - 1

stay :: Int -> Int
stay = id

fromIndex :: Int -> Int -> Point
fromIndex cols index = (x, y)
  where x = index `rem` cols
        y = index `div` cols

