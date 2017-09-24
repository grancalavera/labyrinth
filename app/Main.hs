module Main where
import Lib
import Control.Lens

main :: IO ()
main = do
  drawBoard origin sillyBoard
  board <- initialBoard
  drawBoard origin board

origin = Coords {_x = 0, _y = 1}

a = main
