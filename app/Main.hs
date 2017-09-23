module Main where
import System.Console.ANSI
import System.Random (randomRIO)
import Lib

main :: IO ()
main = do
  quickDraw []

quickDraw :: [Tile] -> IO ()
quickDraw tiles = do
  clearScreen
  mapM_ draw fixedTiles
  mapM_ draw tiles
  setCursorPosition (3*9) 0

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
  preview tile'
