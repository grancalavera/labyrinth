module Labyrinth.UI where

import           Control.Monad.IO.Class (liftIO)
import           Control.Monad          (void)
import           Lens.Micro             ((^.))
import           Brick.Widgets.Core     (str, translateBy)
import           Brick.Main             ( App(..)
                                        , defaultMain
                                        , resizeOrQuit
                                        , neverShowCursor
                                        )
import           Brick.Types            (Widget, Location(..), EventM)
import           Brick.AttrMap          (attrMap)
import qualified Graphics.Vty           as V
import           Data.List              (intercalate)
import           Labyrinth.Tile         ( Tile(..)
                                        , Terrain(..)
                                        , Direction(..)
                                        )
import qualified Labyrinth.Board        as Board
import           Labyrinth.Board        (Position)
import qualified Labyrinth.Game         as Game
import           Labyrinth.Game         (Game, board)
import           Labyrinth.Cell         (Cell, tile)

main :: IO ()
main = void $ defaultMain app mempty

app :: App Game e ()
app = App { appDraw = drawUI
          , appHandleEvent  = resizeOrQuit
          , appStartEvent   = startEvent
          , appAttrMap      = const $ attrMap V.defAttr []
          , appChooseCursor = neverShowCursor
          }

startEvent :: Game -> EventM () Game
startEvent _ = liftIO Game.initialGame

drawUI :: Game -> [Widget ()]
drawUI g = map toTile (Board.toList $ g ^. board)
  where
    toTile :: (Position, Cell) -> Widget ()
    toTile ((x, y), c) = translateBy (Location (x*7, y*3)) (fromTile $ c ^. tile)

fromTile :: Tile -> Widget ()
fromTile t = str $ intercalate "\n" $ case t of
  Tile Gate North   ->  ["       ",
                         "   ▲   ",
                         "       "]
  Tile Gate South   ->  ["       ",
                         "   ▼   ",
                         "       "]
  Tile Gate West    ->  ["       ",
                         "  ◄    ",
                         "       "]
  Tile Gate East    ->  ["       ",
                         "    ►  ",
                         "       "]
  Tile Corner North ->  ["─┘   │ ",
                         "     │ ",
                         "─────┘ "]
  Tile Corner West  ->  ["─────┐ ",
                         "     │ ",
                         "─┐   │ "]
  Tile Corner East  ->  [" │   └─",
                         " │     ",
                         " └─────"]
  Tile Corner South ->  [" ┌─────",
                         " │     ",
                         " │   ┌─"]
  Tile Fork East    ->  [" │   └─",
                         " │     ",
                         " │   ┌─"]
  Tile Fork West    ->  ["─┘   │ ",
                         "     │ ",
                         "─┐   │ "]
  Tile Fork North   ->  ["─┘   └─",
                         "       ",
                         "───────"]
  Tile Fork South   ->  ["───────",
                         "       ",
                         "─┐   ┌─"]
  Tile Path North -> vpath
  Tile Path South -> vpath
  Tile Path West  -> hpath
  Tile Path East  -> hpath
  where
    vpath =             [" │   │ ",
                         " │   │ ",
                         " │   │ "]
    hpath =             ["───────",
                         "       ",
                         "───────"]
