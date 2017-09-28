module Main where
import Lib
import Control.Lens
import System.Console.ANSI
import Control.Monad (forM_)

main :: IO ()
main = do
  clearScreen
  board <- initialBoard
  drawBackground
  drawBoard origin board
  -- this should go eslewhere
  setCursorPosition gameHeight 0
  putStrLn $ replicate 80 '-'

-- this coordinates are given in Tile units
-- it might make sense to use actual columns
-- and rows for certain operations

origin = Coords {_x = 1, _y = 1}

colors :: [Color]
colors = [Yellow, Blue, Red, Green]

colorTest :: IO ()
colorTest = do
  forM_ colors $ \color -> do
    setSGR [SetColor Foreground Vivid White]
    setSGR [SetColor Background Dull color]
    putStrLn $ show color
    setSGR [Reset]

-- Black
-- Red
-- Green
-- Yellow
-- Blue
-- Magenta
-- Cyan
-- White
