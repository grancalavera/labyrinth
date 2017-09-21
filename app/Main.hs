module Main where

import Lib
import System.Console.ANSI

main :: IO ()
main = do
  quickDraw []

quickDraw :: [Tile] -> IO ()
quickDraw tiles = do
  clearScreen
  mapM_ drawTile board
  mapM_ drawTile tiles
  setCursorPosition (3*9) 0

borderStops :: [Int]
borderStops = [1,3,5,7]

gateStops :: [Int]
gateStops = [2,4,6]

forkStops :: [Int]
forkStops = [3, 5]

randomTileStops :: [(Int,Int)]
randomTileStops =    []
                  ++ [(x,y) | x <- [2,4,6], y <- [1,3,5,7]]
                  ++ [(x,y) | x <- [1..7], y <- [2,4,6]]

board :: [Tile]
board = []

        ++ map (\x -> rotateTileTwice  $ makeGate x 0) gateStops
        ++ map (\x ->                    makeGate x 8) gateStops
        ++ map (\y -> rotateTileThrice $ makeGate 0 y) gateStops
        ++ map (\y -> rotateTileOnce   $ makeGate 8 y) gateStops

        ++ [rotateTileTwice  $ makeCornerPath 1 1,
            rotateTileOnce   $ makeCornerPath 7 1,
            rotateTileThrice $ makeCornerPath 1 7,
                               makeCornerPath 7 7]

        ++ map (\x -> rotateTileTwice  $ makeForkPath x 1) forkStops
        ++ map (\x ->                    makeForkPath x 7) forkStops
        ++ map (\y -> rotateTileThrice $ makeForkPath 1 y) forkStops
        ++ map (\y -> rotateTileOnce   $ makeForkPath 7 y) forkStops

        ++ [rotateTileThrice $ makeForkPath 3 3,
                               makeForkPath 3 5,
            rotateTileTwice  $ makeForkPath 5 3,
            rotateTileOnce   $ makeForkPath 5 5]
