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
