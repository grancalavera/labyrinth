module Draw where
import Data.List (sort)
import System.Console.ANSI
import Control.Lens hiding (preview)
import Tile
import Board

drawBoard :: Coords -> Board -> IO ()
drawBoard position board = do
  clearScreen
  mapM_ draw $ (view tiles) board'
  setCursorPosition cursorY 0
  where board' = overCoords (moveCoordsBy position) board
        boardRows = view rows $ board
        boardY = view y position
        cursorY = boardRows * tileHeight + boardY * tileHeight

draw :: Tile -> IO ()
draw tile = drawTileRow 0 (shape tile)
  where drawTileRow :: Int -> Shape -> IO ()
        drawTileRow _ [] = return ()
        drawTileRow rowIndex (row:rows) = do
          setCursorPosition (rowIndex+ty) tx
          putStr row
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

tileWidth :: Int
tileWidth = 7

tileHeight :: Int
tileHeight = 3

tileX :: Tile -> Int
tileX = view (coords.x)

tileY :: Tile -> Int
tileY = view (coords.y)

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
