module Main where
import Lib
import Control.Lens
import System.Console.ANSI

{-  game loop sketch

data World = World {}

main :: IO ()
world <- initialWorld
tick world

tick :: World -> IO ()
tick world = do
  render world
  input <- processInput world
  updatedWorld <- update world input
  case (gameOver world) of
    True -> endGame
    False -> tick updatedWorld
-}


main :: IO ()
main = do
  clearScreen
  drawBackground

  board <- initialBoard
  drawBoard origin board

  -- this should go eslewhere
  setCursorPosition gameHeight 0

-- these coordinates are given in Tile units
-- it might make sense to use actual columns
-- and rows for certain operations
origin = Coords {_x = 1, _y = 1}

