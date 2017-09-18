module Tiles where
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
    [West]  -> ["│      ",
                "│      ",
                "│      "]
    [East]  -> ["      │",
                "      │",
                "      │"]
  Corner -> case tileEdges of
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
  Gate   -> case tileEdges of
    [North] -> ["   ▲   ",
                "       ",
                "───────"]
    [South] -> ["───────",
                "       ",
                "   ▼   "]
    [West]  -> ["      │",
                "◄     │",
                "      │"]
    [East]  -> ["│      ",
                "│     ►",
                "│      "]
  StraightPath -> case tileEdges of
    _ -> []
  CornerPath -> case tileEdges of
    _ -> []
  ForkPath -> case tileEdges of
    _ -> []
  where tileKind = _kind tile
        tileEdges = _edges tile
