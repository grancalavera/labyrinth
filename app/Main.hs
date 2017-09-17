module Main where

import Lib
import System.Console.ANSI

main :: IO ()
main = do
  clearScreen

  drawTile (tileShape Corner [North, West]) (0,0)
  drawTile (tileShape Corner [North, East]) (8,0)

  drawTile (tileShape Corner [South, West]) (0,8)
  drawTile (tileShape Corner [South, East]) (8,8)

  drawTile (tileShape Border [North]) (1,0)
  drawTile (tileShape Border [North]) (2,0)
  drawTile (tileShape Border [North]) (3,0)
  drawTile (tileShape Border [North]) (4,0)
  drawTile (tileShape Border [North]) (5,0)
  drawTile (tileShape Border [North]) (6,0)
  drawTile (tileShape Border [North]) (7,0)

  drawTile (tileShape Border [West]) (0, 1)
  drawTile (tileShape Border [West]) (0, 2)
  drawTile (tileShape Border [West]) (0, 3)
  drawTile (tileShape Border [West]) (0, 4)
  drawTile (tileShape Border [West]) (0, 5)
  drawTile (tileShape Border [West]) (0, 6)
  drawTile (tileShape Border [West]) (0, 7)

  drawTile (tileShape Border [East]) (8, 1)
  drawTile (tileShape Border [East]) (8, 2)
  drawTile (tileShape Border [East]) (8, 3)
  drawTile (tileShape Border [East]) (8, 4)
  drawTile (tileShape Border [East]) (8, 5)
  drawTile (tileShape Border [East]) (8, 6)
  drawTile (tileShape Border [East]) (8, 7)

  drawTile (tileShape Border [South]) (1,8)
  drawTile (tileShape Border [South]) (2,8)
  drawTile (tileShape Border [South]) (3,8)
  drawTile (tileShape Border [South]) (4,8)
  drawTile (tileShape Border [South]) (5,8)
  drawTile (tileShape Border [South]) (6,8)
  drawTile (tileShape Border [South]) (7,8)

  setCursorPosition (3*9) 0
  putStrLn ""
