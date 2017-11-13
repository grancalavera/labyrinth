module Labyrinth.UI where

import           Brick.Widgets.Core (str, translateBy)
import           Brick.Main         (App(..), defaultMain, resizeOrQuit, neverShowCursor)
import           Brick.Types        (Widget, Location(..))
import           Brick.AttrMap      (attrMap)
import qualified Graphics.Vty       as V
import           Data.List          (intercalate)
import           Lens.Micro         ((^.))
import qualified Data.Set           as Set

import           Labyrinth.Tile     ( Tile
                                    , Terrain(..)
                                    , Edge(..)
                                    , terrain
                                    , edges
                                    )
import qualified Labyrinth.Board    as Board
import           Labyrinth.Board    (Position)

main :: IO ()
main = defaultMain app ()

app :: App () e ()
app = App { appDraw = const ui
          , appHandleEvent = resizeOrQuit
          , appStartEvent = return
          , appAttrMap = const $ attrMap V.defAttr []
          , appChooseCursor = neverShowCursor
          }

ui :: [Widget ()]
ui = map toTile (Board.toList Board.fixedTiles)
  where
    toTile :: (Position, Tile) -> Widget ()
    toTile ((x, y), tile) = translateBy (Location (x*7, y*3)) (fromTile tile)

fromTile :: Tile -> Widget ()
fromTile t = str $ intercalate "\n" $ case (tileTerrain, tileEdges) of
  (Blank, _) ->                   ["       ",
                                   "       ",
                                   "       "]
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
  where
    tileTerrain = t ^. terrain
    tileEdges   = Set.toList (t ^. edges)
