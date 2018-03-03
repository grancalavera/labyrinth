module Labyrinth.UI.Game (playGame) where

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
                                        )
import qualified Graphics.Vty           as V
import           Data.List              (intercalate)
import qualified Labyrinth              as Labyrinth
import           Labyrinth              (Position)
import           Labyrinth.Players      (Players, color)
import           Labyrinth.Direction    (Direction(..))
import           Labyrinth.Gate         (Gate(..))
import           Labyrinth.Tile         ( Tile(..)
                                        , Terrain(..)
                                        , direction
                                        , terrain
                                        , tenants
                                        , goal
                                        )
import qualified Labyrinth.Game         as Game
import           Labyrinth.Game         ( Game
                                        , gates
                                        , tiles
                                        , rowSpread
                                        , colSpread
                                        )
import qualified Labyrinth.Goal         as Goal
import           Labyrinth.Goal         (Goal(..), Treasure(..))

playGame :: Players -> IO ()
playGame ps = do
  g <- Game.initialGame ps
  void $ Brick.defaultMain app g

app :: App Game e ()
app = App { appDraw = drawUI
          , appHandleEvent  = Brick.resizeOrQuit
          , appStartEvent   = return
          , appAttrMap      = const $ Brick.attrMap V.defAttr []
          , appChooseCursor = Brick.neverShowCursor
          }

drawUI :: Game -> [Widget ()]
drawUI g =
  [ C.vCenter $ C.hCenter
              $ Brick.vBox
              $ map (Brick.hBox . (map (fromRaw . snd))) (Labyrinth.toRows board')
  ]
  where
    gates' :: Map Position RawCell
    gates' = Map.map toRawGate (g ^. gates)
    tiles' :: Map Position RawCell
    tiles' = Map.map toRawTile (g ^. tiles)
    board' :: Map Position RawCell
    board' = tiles' <> gates' <> (rawEmptyBoard (g ^. rowSpread) (g ^. colSpread))

toRawGate :: Gate -> RawCell
toRawGate (Gate d _) = RawCell $ case d of
  North -> ["         ",
            "   ▲ ▲   ",
            "         ",
            "         "]
  West  -> ["         ",
            "   ◄     ",
            "   ◄     ",
            "         "]
  South -> ["         ",
            "         ",
            "   ▼ ▼   ",
            "         "]
  East  -> ["         ",
            "     ►   ",
            "     ►   ",
            "         "]

toRawTile :: Tile -> RawCell
toRawTile t =  mempty
            <> toRawPlayers  t
            <> toRawFound    t
            <> toRawTreasure t
            <> toRawTerrain  t

toRawPlayers :: Tile -> RawCell
toRawPlayers t = fromMaybe mempty $ do
  guard $ (not . null) players'
  return $ RawCell $ mergeTiles left right
    where
      players' = t ^. tenants
      left   = [emptyRow, emptyRow, emptyRow, emptyRow]
      right  = (map (\p -> (show $ p ^. color) ++ repeat ' ') players') ++ (repeat emptyRow)

toRawTreasure :: Tile -> RawCell
toRawTreasure t = fromMaybe mempty $ do
  (Goal t' _) <- t ^. goal
  c           <- Map.lookup t' treasureMap
  return $ RawCell ["         ",
                    "         ",
                    "    " ++ [c] ++ "    ",
                    "         "]

treasureMap :: Map Treasure Char
treasureMap = Map.fromList $ zip Goal.treasures ['A'..]

toRawFound :: Tile -> RawCell
toRawFound t = fromMaybe mempty $ do
  (Goal _ isFound) <- t ^. goal
  guard isFound
  return $ RawCell ["         ",
                    "         ",
                    "    ✓    ",
                    "         "]

toRawTerrain :: Tile -> RawCell
toRawTerrain t = RawCell $ case (t ^. terrain, t ^. direction) of
  (Path, North)   -> [" │     │ ",
                      " │     │ ",
                      " │     │ ",
                      " │     │ "]
  (Path, West)    -> ["─────────",
                      "         ",
                      "         ",
                      "─────────"]
  (Path, South)   -> [" │     │ ",
                      " │     │ ",
                      " │     │ ",
                      " │     │ "]
  (Path, East)    -> ["─────────",
                      "         ",
                      "         ",
                      "─────────"]
  (Corner, North) -> ["─┘     │ ",
                      "       │ ",
                      "       │ ",
                      "───────┘ "]
  (Corner, West)  -> ["───────┐ ",
                      "       │ ",
                      "       │ ",
                      "─┐     │ "]
  (Corner, South) -> [" ┌───────",
                      " │       ",
                      " │       ",
                      " │     ┌─"]
  (Corner, East)  -> [" │     └─",
                      " │       ",
                      " │       ",
                      " └───────"]
  (Fork, North)   -> ["─┘     └─",
                      "         ",
                      "         ",
                      "─────────"]
  (Fork, West)    -> ["─┘     │ ",
                      "       │ ",
                      "       │ ",
                      "─┐     │ "]
  (Fork, South)   -> ["─────────",
                      "         ",
                      "         ",
                      "─┐     ┌─"]
  (Fork, East)    -> [" │     └─",
                      " │       ",
                      " │       ",
                      " │     ┌─"]

emptyRow :: String
emptyRow = replicate 9 ' '

newtype RawCell = RawCell [String]

instance Monoid RawCell where
  mempty = RawCell $ replicate 4 emptyRow
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

rawEmptyBoard :: [Int] -> [Int] -> Map Position RawCell
rawEmptyBoard rs cs = Map.fromList [((x,y), mempty) | x <- rs, y <- cs]
