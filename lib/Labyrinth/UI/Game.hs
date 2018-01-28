module Labyrinth.UI.Game (playGame) where

import           Control.Monad.IO.Class (liftIO)
import           Control.Monad          (void, guard)
import           Data.Monoid            ((<>))
import           Data.Maybe             (fromMaybe)
import qualified Data.Map               as Map
import           Data.Map               (Map)
import           Lens.Micro             ((^.))
import qualified Brick                  as Brick
import qualified Brick.Widgets.Center   as C
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
                                        , goal
                                        )
import qualified Labyrinth.Game         as Game
import           Labyrinth.Game         ( Game
                                        , gates
                                        , tiles
                                        , rowSpread
                                        , colSpread
                                        )
import qualified Labyrinth.Board        as Board
import           Labyrinth.Board        (Board)
import qualified Labyrinth.Goal         as Goal
import           Labyrinth.Goal         (Goal(..), Treasure(..))

playGame :: IO ()
playGame = void $ Brick.defaultMain app mempty

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
  [ C.vCenter $ C.hCenter
              $ Brick.vBox
              $ map (Brick.hBox . (map (fromRaw . snd))) (Board.toRows board')
  ]
  where
    gates' :: Board RawCell
    gates' = Board.map toRawGate (g ^. gates)
    tiles' :: Board RawCell
    tiles' = Board.map toRawTile (g ^. tiles)
    board' :: Board RawCell
    board' = tiles' <> gates' <> (rawEmptyBoard (g ^. rowSpread) (g ^. colSpread))

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
toRawTile t = toRawFound t <> toRawTreasure t <> toRawTerrain t

toRawTreasure :: Tile -> RawCell
toRawTreasure t = fromMaybe mempty $ do
  (Goal t' _) <- t ^. goal
  c           <- Map.lookup t' treasureMap
  return $ RawCell ["       ",
                    "   " ++ [c] ++ "   ",
                    "       "]

treasureMap :: Map Treasure Char
treasureMap = Map.fromList $ zip Goal.treasures ['A'..]

toRawFound :: Tile -> RawCell
toRawFound t = fromMaybe mempty $ do
  (Goal _ isFound) <- t ^. goal
  guard isFound
  return $ RawCell ["       ",
                    "   ✓   ",
                    "       "]

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
                      " └─────"]
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
  mempty = RawCell ["       ",
                    "       ",
                    "       "]
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

rawEmptyBoard :: [Int] -> [Int] -> Board RawCell
rawEmptyBoard rs cs = Board.fromList [((x,y), mempty) | x <- rs, y <- cs]
