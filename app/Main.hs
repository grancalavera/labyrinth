module Main where
import Lib
import System.Console.ANSI

main :: IO ()
main = do
  clearScreen
  drawBackground

  board <- initialBoard
  drawBoard origin board

  -- this should go elsewhere
  setCursorPosition gameHeight 0

-- these coordinates are given in Tile units
-- it might make sense to use actual columns
-- and rows for certain operations
origin = Coords {_x = 1, _y = 1}

