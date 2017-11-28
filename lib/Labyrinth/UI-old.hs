module Labyrinth.UI
    ( ui
    , previewRotation
    ) where

import Data.List (sort)
import System.Console.ANSI
import Lens.Micro ((^.))

import Labyrinth

ui :: IO ()
ui = do
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

draw :: Tile -> IO ()
draw tile = drawTileRow 0 (shape tile)
  where drawTileRow :: Int -> Shape -> IO ()
        drawTileRow _ [] = return ()
        drawTileRow rowIndex (r:rs) = do
          setCursorPosition (rowIndex+ty) tx
          setSGR [SetColor Background Dull Black]
          setSGR [SetColor Foreground Vivid White]
          putStr r
          setSGR [Reset]
          drawTileRow (rowIndex+1) rs
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
background :: String

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
  (_, _) ->                       error "unknown tile"
  where tileKind = _kind tile
        tileEdges = sort $ _edges tile