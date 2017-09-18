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
tileShape tile = case (tileKind, tileEdges) of
  (Gate, [North]) ->                  ["   ▲   ",
                                       "       ",
                                       "       "]
  (Gate, [South]) ->                  ["       ",
                                       "       ",
                                       "   ▼   "]
  (Gate, [West]) ->                   ["       ",
                                       "◄      ",
                                       "       "]
  (Gate, [East]) ->                   ["       ",
                                       "      ►",
                                       "       "]
  (CornerPath, [North, West]) ->      ["─┘   │ ",
                                       "     │ ",
                                       "─────┘ "]
  (CornerPath, [North, East]) ->      [" │   └─",
                                       " │     ",
                                       " └─────"]
  (CornerPath, [South, East]) ->      [" ┌─────",
                                       " │     ",
                                       " │   ┌─"]
  (CornerPath, [West, South]) ->      ["─────┐ ",
                                       "     │ ",
                                       "─┐   │ "]
  (ForkPath, [North, South, East]) -> [" │   └─",
                                       " │     ",
                                       " │   ┌─"]
  (ForkPath, [North, West, South]) -> ["─┘   │ ",
                                       "     │ ",
                                       "─┐   │ "]
  (ForkPath, [North, West, East]) ->  ["─┘   └─",
                                       "       ",
                                       "───────"]
  (ForkPath, [West, South, East]) ->  ["───────",
                                       "       ",
                                       "─┐   ┌─"]
  (StraightPath, [North, South]) ->   [" │   │ ",
                                       " │   │ ",
                                       " │   │ "]
  (StraightPath, [West, East]) ->     ["───────",
                                       "       ",
                                       "───────"]
  where tileKind = _kind tile
        tileEdges = sort $ _edges tile
