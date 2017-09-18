module Main where

import Lib
import System.Console.ANSI

main :: IO ()
main = do
  clearScreen
  mapM_ drawTile board
  setCursorPosition (3*9) 0
  putStrLn ""

borderStops :: [Int]
borderStops = [1,3,5,7]

gateStops :: [Int]
gateStops = [2,4,6]

board :: [Tile]
board = []

        ++ [                   makeCorner 0 0,
            rotateTileThrice $ makeCorner 8 0,
            rotateTileOnce   $ makeCorner 0 8,
            rotateTileTwice  $ makeCorner 8 8]

        ++ map (\x ->                    makeBorder x 0) borderStops
        ++ map (\x -> rotateTileTwice  $ makeBorder x 8) borderStops
        ++ map (\y -> rotateTileOnce   $ makeBorder 0 y) borderStops
        ++ map (\y -> rotateTileThrice $ makeBorder 8 y) borderStops

        ++ map (\x -> rotateTileTwice  $ makeGate x 0) gateStops
        ++ map (\x ->                    makeGate x 8) gateStops
        ++ map (\y -> rotateTileThrice $ makeGate 0 y) gateStops
        ++ map (\y -> rotateTileOnce   $ makeGate 8 y) gateStops

        ++ [rotateTileTwice  $ makeCornerPath 1 1,
            rotateTileOnce   $ makeCornerPath 7 1,
            rotateTileThrice $ makeCornerPath 1 7,
                               makeCornerPath 7 7]
