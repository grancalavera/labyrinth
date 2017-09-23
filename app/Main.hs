module Main where
import Lib

main :: IO ()
main = do
  board <- initialBoard
  drawBoard (Coords {_x=0,_y=0}) board

