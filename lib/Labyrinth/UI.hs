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
import           Labyrinth.Gate         (Gate(..))
import qualified Labyrinth.Game         as Game
import           Labyrinth.Game         (Game, gates)
import qualified Labyrinth.Board        as Board
import           Labyrinth.Board        (Board)

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
  [ Brick.vBox $ map (Brick.hBox . (map (fromRaw . snd))) (Board.toRows board')
  ]
  where
    gates' :: Board [String]
    gates' = Board.map toRawGate (g ^. gates)
    board' :: Board [String]
    board' = gates' <> rawEmptyBoard

toRawGate :: Gate -> [String]
toRawGate (Gate North _) = ["       ",
                            "   ▲   ",
                            "       "]
toRawGate (Gate West _)  = ["       ",
                            "  ◄    ",
                            "       "]
toRawGate (Gate South _) = ["       ",
                            "   ▼   ",
                            "       "]
toRawGate (Gate East _)  = ["       ",
                            "    ►  ",
                            "       "]

-- widgetFromTile :: Cell Tile -> Widget ()
-- widgetFromTile Empty = empty
-- widgetFromTile (Cell d (Tile t _)) = fromRaw $ case (t, d) of
--   (Path, North)   -> [" │   │ ",
--                       " │   │ ",
--                       " │   │ "]
--   (Path, West)    -> ["───────",
--                       "       ",
--                       "───────"]
--   (Path, South)   -> [" │   │ ",
--                       " │   │ ",
--                       " │   │ "]
--   (Path, East)    -> ["───────",
--                       "       ",
--                       "───────"]
--   (Corner, North) -> ["─┘   │ ",
--                       "     │ ",
--                       "─────┘ "]
--   (Corner, West)  -> ["─────┐ ",
--                       "     │ ",
--                       "─┐   │ "]
--   (Corner, South) -> [" ┌─────",
--                       " │     ",
--                       " │   ┌─"]
--   (Corner, East)  -> [" │   └─",
--                       " │     ",
--                       " │   ┌─"]
--   (Fork, North)   -> ["─┘   └─",
--                       "       ",
--                       "───────"]
--   (Fork, West)    -> ["─┘   │ ",
--                       "     │ ",
--                       "─┐   │ "]
--   (Fork, South)   -> ["───────",
--                       "       ",
--                       "─┐   ┌─"]
--   (Fork, East)    -> [" │   └─",
--                       " │     ",
--                       " │   ┌─"]

empty :: Widget ()
empty = fromRaw rawEmpty

fromRaw :: [String] -> Widget ()
fromRaw r = Brick.str (intercalate "\n" r)

choose :: Char -> Char -> Char
choose ' ' c   = c
choose c   ' ' = c
choose _   c   = c

mergeRows :: String -> String -> String
mergeRows = mergeWith choose

mergeTiles :: [String] -> [String] -> [String]
mergeTiles = mergeWith mergeRows

mergeWith :: (a -> a -> a) -> [a] -> [a] -> [a]
mergeWith f xs ys = [f x y | (x, y) <- zip xs ys]

rawEmpty :: [String]
rawEmpty = replicate 3 "       "

rawEmptyBoard :: Board [String]
rawEmptyBoard = Board.fromList [((x,y), rawEmpty) | x <- size, y <- size]
  where
    size :: [Int]
    size = [0..8]
