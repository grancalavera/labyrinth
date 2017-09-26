module Main where
import Lib
import Control.Lens
import System.Console.ANSI
import Control.Monad (forM_)

main :: IO ()
main = boardTest

boardTest :: IO ()
boardTest = do
  drawBoard origin sillyBoard
  board <- initialBoard
  drawBoard origin board

origin = Coords {_x = 0, _y = 1}

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
