module Labyrinth.UI where

import           Control.Monad.IO.Class (liftIO)
import           Control.Monad          (void)
import           Data.Monoid            ((<>))
import           Data.Maybe             (fromMaybe)
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
import           Labyrinth.Tile         ( Tile(..)
                                        , Terrain(..)
                                        , direction
                                        , terrain
                                        )
import qualified Labyrinth.Game         as Game
import           Labyrinth.Game         (Game, gates, tiles)
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
    gates' :: Board RawCell
    gates' = Board.map toRawGate (g ^. gates)
    tiles' :: Board RawCell
    tiles' = Board.map toRawTile (g ^. tiles)
    board' :: Board RawCell
    board' = tiles' <> gates' <> rawEmptyBoard

toRawGate :: Gate -> RawCell
toRawGate (Gate d _) = RawCell $ case d of
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

toRawTile :: Tile -> RawCell
toRawTile t = toRawTreasure t <> toRawFound t <> toRawTerrain t

toRawTreasure :: Tile -> RawCell
toRawTreasure _ = fromMaybe mempty Nothing

toRawFound :: Tile -> RawCell
toRawFound _ = fromMaybe mempty Nothing

toRawTerrain :: Tile -> RawCell
toRawTerrain t = RawCell $ case (t ^. terrain, t ^. direction) of
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

data RawCell = RawCell [String]

instance Monoid RawCell where
  mempty = RawCell $ replicate 3 "       "
  RawCell l `mappend` RawCell r = RawCell $ mergeTiles l r

fromRaw :: RawCell -> Widget ()
fromRaw (RawCell r) = Brick.str (intercalate "\n" r)

choose :: Char -> Char -> Char
choose ' ' c   = c
choose c   ' ' = c
choose c   _   = c

mergeRows :: String -> String -> String
mergeRows = mergeWith choose

mergeTiles :: [String] -> [String] -> [String]
mergeTiles = mergeWith mergeRows

mergeWith :: (a -> a -> a) -> [a] -> [a] -> [a]
mergeWith f xs ys = [f x y | (x, y) <- zip xs ys]

rawEmptyBoard :: Board RawCell
rawEmptyBoard = Board.fromList [((x,y), mempty) | x <- size, y <- size]
  where
    size :: [Int]
    size = [0..8]
