module UI (main) where

import System.Console.ANSI
import Data.List (sort)
import Control.Monad (forM_)
import System.Console.ANSI
import Lens.Micro ((^.))

import Labyrinth

main :: IO ()
main = do
  clearScreen
  drawBackground
  board <- initialBoard
  drawBoard Coords {_x = 1, _y = 1} board
  setCursorPosition gameHeight 0

drawBackground :: IO ()
drawBackground = do
  setCursorPosition 0 0
  setSGR [SetColor Background Dull Black]
  putStr background
  setSGR [Reset]

drawBoard :: Coords -> Board -> IO ()
drawBoard position board = do
  mapM_ draw $ board' ^. tiles
  where board' = overCoords (moveCoordsBy position) board
        boardRows = board ^. rows
        boardY = position ^. y

draw :: Tile -> IO ()
draw tile = drawTileRow 0 (shape tile)
  where drawTileRow :: Int -> Shape -> IO ()
        drawTileRow _ [] = return ()
        drawTileRow rowIndex (row:rows) = do
          setCursorPosition (rowIndex+ty) tx
          setSGR [SetColor Background Dull Black]
          setSGR [SetColor Foreground Vivid White]
          putStr row
          setSGR [Reset]
          drawTileRow (rowIndex+1) rows
        tx = (tileX tile) * tileWidth
        ty = (tileY tile) * tileHeight

preview :: Tile -> IO ()
preview tile = do
  clearScreen
  draw tile
  setCursorPosition (tileY tile * tileHeight + tileHeight + 1) 0

previewRotation :: Tile -> IO ()
previewRotation tile = do
  tile' <-  rotateTileRandomly tile
  preview tile'

-- row/col units

gameWidth :: Int
gameWidth = 77

gameHeight :: Int
gameHeight = 40

background = unlines $ replicate gameHeight $ replicate gameWidth ' '

-- Tile units
tileWidth :: Int
tileWidth = 7

tileHeight :: Int
tileHeight = 3

tileX :: Tile -> Int
tileX t = t ^. coords.x

tileY :: Tile -> Int
tileY t = t ^. coords.y

type Shape = [String]

shape :: Tile -> Shape
shape tile = case (tileKind, tileEdges) of
  (Gate, [North]) ->              ["       ",
                                   "   ▲   ",
                                   "       "]
  (Gate, [South]) ->              ["       ",
                                   "   ▼   ",
                                   "       "]
  (Gate, [West]) ->               ["       ",
                                   "  ◄    ",
                                   "       "]
  (Gate, [East]) ->               ["       ",
                                   "    ►  ",
                                   "       "]
  (Corner, [North, West]) ->      ["─┘   │ ",
                                   "     │ ",
                                   "─────┘ "]
  (Corner, [North, East]) ->      [" │   └─",
                                   " │     ",
                                   " └─────"]
  (Corner, [South, East]) ->      [" ┌─────",
                                   " │     ",
                                   " │   ┌─"]
  (Corner, [West, South]) ->      ["─────┐ ",
                                   "     │ ",
                                   "─┐   │ "]
  (Fork, [North, South, East]) -> [" │   └─",
                                   " │     ",
                                   " │   ┌─"]
  (Fork, [North, West, South]) -> ["─┘   │ ",
                                   "     │ ",
                                   "─┐   │ "]
  (Fork, [North, West, East]) ->  ["─┘   └─",
                                   "       ",
                                   "───────"]
  (Fork, [West, South, East]) ->  ["───────",
                                   "       ",
                                   "─┐   ┌─"]
  (Path, [North, South]) ->       [" │   │ ",
                                   " │   │ ",
                                   " │   │ "]
  (Path, [West, East]) ->         ["───────",
                                   "       ",
                                   "───────"]
  where tileKind = _kind tile
        tileEdges = sort $ _edges tile

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
