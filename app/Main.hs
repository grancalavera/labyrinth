module Main where

import Lib
import System.Console.ANSI

drawTest = draw ["12.","34.","..."]

main :: IO ()
main = do
  clearScreen

  drawTest  (0,0)
  drawTest  (3,0)
  drawTest  (6,0)

  drawTest  (0,3)
  drawTest  (3,3)
  drawTest  (6,3)

  putStrLn ""
