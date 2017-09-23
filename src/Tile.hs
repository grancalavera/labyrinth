module Tile where

import Data.List (sort)
import System.Console.ANSI
import Control.Lens

import Labyrinth

type Shape = [String]

tileWidth :: Int
tileWidth = 7

tileHeight :: Int
tileHeight = 3

draw :: Tile -> IO ()
draw tile = drawTileRow 0 (shape tile)
  where drawTileRow :: Int -> Shape -> IO ()
        drawTileRow _ [] = return ()
        drawTileRow rowIndex (row:rows) = do
          setCursorPosition (rowIndex+tileY) tileX
          putStr row
          drawTileRow (rowIndex+1) rows

        tileX = (view (coords.x) tile) * tileWidth
        tileY = (view (coords.y) tile) * tileHeight

shape :: Tile -> Shape
shape tile = case (tileKind, tileEdges) of
  (Gate, [North]) ->              ["   ▲   ",
                                   "       ",
                                   "       "]
  (Gate, [South]) ->              ["       ",
                                   "       ",
                                   "   ▼   "]
  (Gate, [West]) ->               ["       ",
                                   "◄      ",
                                   "       "]
  (Gate, [East]) ->               ["       ",
                                   "      ►",
                                   "       "]
  (Corner, [North, West]) ->      ["─┘   │ ",
                                   "     │ ",
                                   "─────┘ "]
  (Corner, [North, East]) ->      [" │   └─",
                                   " │     ",
                                   " └─────"]
  (Corner, [South, East]) ->      [" ┌─────",
                                   " │     ",
                                   " │   ┌─"]
  (Corner, [West, South]) ->      ["─────┐ ",
                                   "     │ ",
                                   "─┐   │ "]
  (Fork, [North, South, East]) -> [" │   └─",
                                   " │     ",
                                   " │   ┌─"]
  (Fork, [North, West, South]) -> ["─┘   │ ",
                                   "     │ ",
                                   "─┐   │ "]
  (Fork, [North, West, East]) ->  ["─┘   └─",
                                   "       ",
                                   "───────"]
  (Fork, [West, South, East]) ->  ["───────",
                                   "       ",
                                   "─┐   ┌─"]
  (Path, [North, South]) ->       [" │   │ ",
                                   " │   │ ",
                                   " │   │ "]
  (Path, [West, East]) ->         ["───────",
                                   "       ",
                                   "───────"]
  where tileKind = _kind tile
        tileEdges = sort $ _edges tile
