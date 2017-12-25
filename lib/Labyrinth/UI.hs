module Labyrinth.UI where
import qualified Data.Map               as Map
import           Data.Map               (Map)
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
import           Labyrinth.Cell         (Cell(..))
import           Labyrinth.Gate         (Gate)
import           Labyrinth.Tile         ( Tile(..)
                                        , Terrain(..)
                                        )
import qualified Labyrinth.Board        as Board
import           Labyrinth.Board        (Board, Position)
import qualified Labyrinth.Game         as Game
import           Labyrinth.Game         (Game, tiles, gates)

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
drawUI = undefined
-- drawUI g =
--   [ Brick.vBox $ boardToWidgetRows $ board'
--   ]
--   where
--     board' = Game.blankBoard <> g ^. gates <> g ^. board

-- boardToWidgetRows :: (Cell a -> Widget()) -> Board a -> [Widget ()]
-- boardToWidgetRows toWidget b = map (toWidgetRow b) [0..8]
--   where
--     toWidgetRow :: Board a -> Int -> Widget ()
--     toWidgetRow b r = Brick.hBox $ map toWidget $ Board.toListByRow r b

-- toWidget :: (Position, Cell) -> Widget ()
-- toWidget (_, c) = widgetFromCell c

toWidgetMap :: (Cell a -> Widget()) -> Board a -> Map Position (Widget ())
toWidgetMap f b = Map.map f $ Board.toMap b

widgetFromGate :: Cell Gate -> Widget ()
widgetFromGate Empty = empty
widgetFromGate (Cell d _) = fromRaw $ case d of
  North -> ["       ",
            "   ▲   ",
            "       "]
  West  -> ["       ",
            "  ◄    ",
            "       "]
  South -> ["       ",
            "   ▼   ",
            "       "]
  East  -> ["       ",
            "    ►  ",
            "       "]

tileFromGate :: Cell Tile -> Widget ()
tileFromGate Empty = empty
tileFromGate (Cell d (Tile t _)) = fromRaw $ case (t, d) of
  (Path, North)   -> [" │   │ ",
                      " │   │ ",
                      " │   │ "]
  (Path, West)    -> ["───────",
                      "       ",
                      "───────"]
  (Path, South)   -> [" │   │ ",
                      " │   │ ",
                      " │   │ "]
  (Path, East)    -> ["───────",
                      "       ",
                      "───────"]
  (Corner, North) -> ["─┘   │ ",
                      "     │ ",
                      "─────┘ "]
  (Corner, West)  -> ["─────┐ ",
                      "     │ ",
                      "─┐   │ "]
  (Corner, South) -> [" ┌─────",
                      " │     ",
                      " │   ┌─"]
  (Corner, East)  -> [" │   └─",
                      " │     ",
                      " │   ┌─"]
  (Fork, North)   -> ["─┘   └─",
                      "       ",
                      "───────"]
  (Fork, West)    -> ["─┘   │ ",
                      "     │ ",
                      "─┐   │ "]
  (Fork, South)   -> ["───────",
                      "       ",
                      "─┐   ┌─"]
  (Fork, East)    -> [" │   └─",
                      " │     ",
                      " │   ┌─"]

empty :: Widget ()
empty = fromRaw $ replicate 3 "       "

fromRaw :: [String] -> Widget ()
fromRaw r = Brick.str (intercalate "\n" r)
