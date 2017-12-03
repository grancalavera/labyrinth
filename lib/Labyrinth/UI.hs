module Labyrinth.UI where

import           Control.Monad.IO.Class (liftIO)
import           Control.Monad          (void, forM)
import           Lens.Micro             ((^.))
import           Brick.Widgets.Core     (str, translateBy)
import           Brick.Main             ( App(..)
                                        , defaultMain
                                        , resizeOrQuit
                                        , neverShowCursor
                                        )
import qualified Brick                  as Brick
import           Brick.Types            (Widget, Location(..), EventM)
import           Brick.AttrMap          (attrMap)
import qualified Graphics.Vty           as V
import           Data.List              (intercalate)
import           Labyrinth.Tile         ( Tile(..)
                                        , Terrain(..)
                                        , Direction(..)
                                        )
import qualified Labyrinth.Board        as Board
import           Labyrinth.Board        (Board, Position)
import qualified Labyrinth.Game         as Game
import           Labyrinth.Game         (Game, board, gates)
import           Labyrinth.Cell         (Cell, tile)


import qualified Data.Map as Map
import Data.Map (Map)


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
drawUI g =
  [ Brick.vBox $ boardToWidgetRows $ g ^. board
  , Brick.vBox $ boardToWidgetRows $ g ^. gates
  ]

boardToWidgetRows :: Board -> [Widget ()]
boardToWidgetRows b = map (toWidgetRow b) [0..8]

toWidgetRow :: Board -> Int -> Widget ()
toWidgetRow b r = Brick.hBox $ map toWidget $ Board.toListByRow r b

toWidget :: (Position, Cell) -> Widget ()
toWidget ((x, y), c) = widgetFromTile $ c ^. tile

widgetFromTile :: Tile -> Widget ()
widgetFromTile t = str $ intercalate "\n" $ case t of
  Tile Blank _      ->  ["       ",
                         "       ",
                         "       "]
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
