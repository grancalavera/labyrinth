module Labyrinth.UI where

import           Control.Monad.IO.Class (liftIO)
import           Control.Monad          (void)
import           Data.Monoid            ((<>))
import           Lens.Micro             ((^.))
import qualified Brick                  as Brick
import           Brick                  ( App(..)
                                        , Widget
                                        , EventM
                                        )
import qualified Graphics.Vty           as V
import           Data.List              (intercalate)
import           Labyrinth.Direction    (Direction(..))
import           Labyrinth.Tile         ( Tile(..)
                                        , Terrain(..)
                                        )
import qualified Labyrinth.Board        as Board
import           Labyrinth.Board        (Board, Position)
import qualified Labyrinth.Game         as Game
import           Labyrinth.Game         (Game, board, gates)
import           Labyrinth.Cell         (Cell, tile)

main :: IO ()
main = void $ Brick.defaultMain app mempty

app :: App Game e ()
app = App { appDraw = drawUI
          , appHandleEvent  = Brick.resizeOrQuit
          , appStartEvent   = startEvent
          , appAttrMap      = const $ Brick.attrMap V.defAttr []
          , appChooseCursor = Brick.neverShowCursor
          }

startEvent :: Game -> EventM () Game
startEvent _ = liftIO Game.initialGame

drawUI :: Game -> [Widget ()]
drawUI g =
  [ Brick.vBox $ boardToWidgetRows $ board'
  ]
  where
    board' = Game.blankBoard <> g ^. gates <> g ^. board

boardToWidgetRows :: Board -> [Widget ()]
boardToWidgetRows b = map (toWidgetRow b) [0..8]

toWidgetRow :: Board -> Int -> Widget ()
toWidgetRow b r = Brick.hBox $ map toWidget $ Board.toListByRow r b

toWidget :: (Position, Cell) -> Widget ()
toWidget (_, c) = widgetFromCell c

widgetFromCell :: Cell -> Widget ()
widgetFromCell c = Brick.str $ intercalate "\n" $ case tile' of
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
    tile' = c ^. tile
    vpath =             [" │   │ ",
                         " │   │ ",
                         " │   │ "]
    hpath =             ["───────",
                         "       ",
                         "───────"]
