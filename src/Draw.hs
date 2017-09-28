module Draw where
import Data.List (sort)
import Control.Monad (forM_)
import System.Console.ANSI
import Control.Lens hiding (preview)
import Tile
import Board

drawBackground :: IO ()
drawBackground = do
  setCursorPosition 0 0
  setSGR [SetColor Background Dull Black]
  putStr background
  setSGR [Reset]

drawBoard :: Coords -> Board -> IO ()
drawBoard position board = do
  mapM_ draw $ (view tiles) board'
  where board' = overCoords (moveCoordsBy position) board
        boardRows = view rows $ board
        boardY = view y position

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
