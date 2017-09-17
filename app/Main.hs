module Main where

import Lib
import System.Console.ANSI

main :: IO ()
main = do
  clearScreen

  draw (tileShape Corner [North, West]) (0,0)
  draw (tileShape Corner [North, East]) (7*8,0)

  draw (tileShape Corner [South, West]) (0,3*8)
  draw (tileShape Corner [South, East]) (7*8,3*8)

  draw (tileShape Border [North]) (7,0)
  draw (tileShape Border [North]) (7*2,0)
  draw (tileShape Border [North]) (7*3,0)
  draw (tileShape Border [North]) (7*4,0)
  draw (tileShape Border [North]) (7*5,0)
  draw (tileShape Border [North]) (7*6,0)
  draw (tileShape Border [North]) (7*7,0)

  draw (tileShape Border [West]) (0, 3)
  draw (tileShape Border [West]) (0, 3*2)
  draw (tileShape Border [West]) (0, 3*3)
  draw (tileShape Border [West]) (0, 3*4)
  draw (tileShape Border [West]) (0, 3*5)
  draw (tileShape Border [West]) (0, 3*6)
  draw (tileShape Border [West]) (0, 3*7)

  draw (tileShape Border [East]) (7*8, 3)
  draw (tileShape Border [East]) (7*8, 3*2)
  draw (tileShape Border [East]) (7*8, 3*3)
  draw (tileShape Border [East]) (7*8, 3*4)
  draw (tileShape Border [East]) (7*8, 3*5)
  draw (tileShape Border [East]) (7*8, 3*6)
  draw (tileShape Border [East]) (7*8, 3*7)

  draw (tileShape Border [South]) (7,3*8)
  draw (tileShape Border [South]) (7*2,3*8)
  draw (tileShape Border [South]) (7*3,3*8)
  draw (tileShape Border [South]) (7*4,3*8)
  draw (tileShape Border [South]) (7*5,3*8)
  draw (tileShape Border [South]) (7*6,3*8)
  draw (tileShape Border [South]) (7*7,3*8)

  setCursorPosition (3*9) 0
  putStrLn ""
