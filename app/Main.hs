module Main where

import Lib
import System.Console.ANSI
import System.Random (randomRIO)

main :: IO ()
main = do
  quickDraw []

quickDraw :: [Tile] -> IO ()
quickDraw tiles = do
  clearScreen
  mapM_ drawTile fixedTiles
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

fixedTiles :: [Tile]
fixedTiles =    []

             ++ map (\x -> rotateTimes 2 $ gate x 0) gateStops
             ++ map (\x ->                 gate x 8) gateStops
             ++ map (\y -> rotateTimes 3 $ gate 0 y) gateStops
             ++ map (\y -> rotateTimes 1 $ gate 8 y) gateStops

             ++ [rotateTimes 2 $ corner 1 1,
                 rotateTimes 1 $ corner 7 1,
                 rotateTimes 3 $ corner 1 7,
                                 corner 7 7]

             ++ map (\x -> rotateTimes 2 $ fork x 1) forkStops
             ++ map (\x ->                 fork x 7) forkStops
             ++ map (\y -> rotateTimes 3 $ fork 1 y) forkStops
             ++ map (\y -> rotateTimes 1 $ fork 7 y) forkStops

             ++ [rotateTimes 3 $ fork 3 3,
                                 fork 3 5,
                 rotateTimes 2 $ fork 5 3,
                 rotateTimes 1 $ fork 5 5]

shuffleList :: [a] -> IO [a]
shuffleList [] = return []
shuffleList list = do
  i <- randomRIO (0, (length list)-1)
  case (splitAt i list) of
    (before, (x:after)) -> do
      xs <- shuffleList (before ++ after)
      return (x:xs)

randomRotation :: IO (Tile -> Tile)
randomRotation = do
  n <- randomRIO (0, 3)
  return (rotateTimes n)

rotateTileRandomly :: Tile -> IO Tile
rotateTileRandomly tile = do
  rotate <- randomRotation
  return (rotate tile)

testTileRotation :: Tile -> IO ()
testTileRotation tile = do
  tile' <-  rotateTileRandomly tile
  clearScreen
  drawTile tile'
  setCursorPosition 5 0

showTile :: Tile -> IO ()
showTile tile = do
  clearScreen
  drawTile tile
  setCursorPosition 5 0
