module Labyrinth.UI where

import           Brick.Widgets.Core (str)
import           Brick.Main         (simpleMain)
import           Brick.Types        (Widget)
import           Data.List          (intercalate)
import           Lens.Micro         ((^.))
import qualified Data.Set           as Set

import qualified Labyrinth.Tile     as Tile
import           Labyrinth.Tile     ( Tile
                                    , Terrain(..)
                                    , Edge(..)
                                    ,terrain
                                    , edges
                                    )

main :: IO ()
main = simpleMain $ fromTile $ Tile.fromTerrain Fork

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
