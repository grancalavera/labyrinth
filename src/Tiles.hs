module Tiles where
import Data.List (sort)
import System.Console.ANSI
import Control.Lens

import Point
import Labyrinth

type Shape = [String]

tileWidth :: Int
tileWidth = 7

tileHeight :: Int
tileHeight = 3

draw :: Shape -> Point -> IO ()
draw = draw' 0
  where draw' :: Int -> Shape -> Point -> IO ()
        draw' _ [] _ = return ()
        draw' row (ln:lns) point@(x,y) = do
          setCursorPosition (row+y) x
          putStr ln
          draw' (row+1) lns point

drawTile :: Tile -> IO ()
drawTile tile = draw shape (tileX, tileY)
  where shape = tileShape tile
        tileX = (view (coords.x) tile) * tileWidth
        tileY = (view (coords.y) tile) * tileHeight

tileShape :: Tile -> Shape
tileShape tile = case tileKind of

  Border -> case tileEdges of
    [North] -> ["───────",
                "       ",
                "       "]
    [South] -> ["       ",
                "       ",
                "───────"]
    [West] ->  ["│      ",
                "│      ",
                "│      "]
    [East] ->  ["      │",
                "      │",
                "      │"]

  Corner -> case tileEdges of
    [North, West] -> ["┌──────",
                      "│      ",
                      "│      "]
    [North, East] ->  ["──────┐",
                       "      │",
                       "      │"]
    [South, East] ->  ["      │",
                       "      │",
                       "──────┘"]
    [West, South] -> ["│      ",
                      "│      ",
                      "└──────"]

  Gate   -> case tileEdges of
    [North] -> ["   ▲   ",
                "       ",
                "───────"]
    [South] -> ["───────",
                "       ",
                "   ▼   "]
    [West] ->  ["      │",
                "◄     │",
                "      │"]
    [East] ->  ["│      ",
                "│     ►",
                "│      "]

  CornerPath -> case tileEdges of
    [North, West] -> ["─┘   │ ",
                      "     │ ",
                      "─────┘ "]
    [North, East] -> [" │   └─",
                      " │     ",
                      " └─────"]
    [South, East] -> [" ┌─────",
                      " │     ",
                      " │   ┌─"]
    [West, South] -> ["─────┐ ",
                      "     │ ",
                      "─┐   │ "]

  ForkPath -> case tileEdges of
    [North, South, East] -> []
    [North, West, South] -> []
    [North, West, East]  -> []
    [West, South, East]  -> []

  StraightPath -> case tileEdges of
    _ -> []

  where tileKind = _kind tile
        tileEdges = sort $ _edges tile
