module Tiles where
import System.Console.ANSI

import Point
import Labyrinth

type Shape = [String]

draw :: Shape -> Point -> IO ()
draw = draw' 0
  where draw' :: Int -> Shape -> Point -> IO ()
        draw' _ [] _ = return ()
        draw' row (ln:lns) point@(x,y) = do
          setCursorPosition (row+y) x
          putStr ln
          draw' (row+1) lns point

tileShape :: TileKind -> [Edge] -> Shape
tileShape Border edges = case edges of
  [North] -> a
  [South] -> b
  [West]  -> c
  [East]  -> d
  where
    a = ["───────",
         "       ",
         "       "]
    b = ["       ",
         "       ",
         "───────"]
    c = ["│      ",
         "│      ",
         "│      "]
    d = ["      │",
         "      │",
         "      │"]
tileShape Corner edges = case edges of
  [North, West] -> a
  [West, North] -> a
  [East, North] -> b
  [North, East] -> b
  [South, East] -> c
  [East, South] -> c
  [West, South] -> d
  [South, West] -> d
  where
    a = ["┌──────",
         "│      ",
         "│      "]
    b = ["──────┐",
         "      │",
         "      │"]
    c = ["      │",
         "      │",
         "──────┘"]
    d = ["│      ",
         "│      ",
         "└──────"]

tileShape _ _ = undefined
