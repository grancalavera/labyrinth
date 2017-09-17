module Main where

import Lib
import System.Console.ANSI

board :: [Tile]
board = [                    makeCorner 0 0
        , rotateTileThrice $ makeCorner 8 0
        , rotateTileOnce   $ makeCorner 0 8
        , rotateTileTwice  $ makeCorner 8 8 ]
        ++ map (\x ->                    makeBorder x 0) [1..7]
        ++ map (\x -> rotateTileTwice  $ makeBorder x 8) [1..7]
        ++ map (\y -> rotateTileOnce   $ makeBorder 0 y) [1..7]
        ++ map (\y -> rotateTileThrice $ makeBorder 8 y) [1..7]

main :: IO ()
main = do
  clearScreen
  mapM_ drawTile board
  setCursorPosition (3*9) 0
  putStrLn ""
