module Labyrinth (main) where

import System.Console.ANSI
import Tile
import Draw
import Board

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
