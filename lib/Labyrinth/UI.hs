module Labyrinth.UI where

-- import           Control.Monad.IO.Class (liftIO)
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
import           Labyrinth.Game         (Game)

main :: IO ()
main = defaultMain app ()

app :: App () e ()
app = App { appDraw = drawUI
          , appHandleEvent = resizeOrQuit
          -- https://github.com/jtdaugherty/brick/blob/master/docs/guide.rst#starting-up-appstartevent
          , appStartEvent = return
          , appAttrMap = const $ attrMap V.defAttr []
          , appChooseCursor = neverShowCursor
          }

startEvent :: Game -> EventM () Game
startEvent _ = return $ (mempty :: Game)

drawUI :: () -> [Widget ()]
drawUI _ = map toTile (Board.toList Board.fixedTiles)
  where
    toTile :: (Position, Tile) -> Widget ()
    toTile ((x, y), tile) = translateBy (Location (x*7, y*3)) (fromTile tile)

fromTile :: Tile -> Widget ()
fromTile tile = str $ intercalate "\n" $ case tile of
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
