module Labyrinth.UI where

import qualified Data.Map               as Map
import           Data.Map               (Map)
import           Control.Monad.IO.Class (liftIO)
import           Control.Monad          (void)
-- import           Data.Monoid            ((<>))
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
import qualified Labyrinth.Game         as Game
import           Labyrinth.Game         (Game, gates)
import           Labyrinth.Board        (Position)

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
  [ Brick.vBox $ map (Brick.hBox . (map (fromRaw . snd))) (toRows board')
  ]
  where
    gates' :: Map Position [String]
    gates' = Map.map toRawGate (g ^. gates)
    board' :: Map Position [String]
    board' = Map.union gates' rawEmptyBoard

toRows :: Map Position a -> [[(Position, a)]]
toRows m = map (Map.toList . (filterByRow m)) (rowSpread m)

rowSpread :: (Map Position a) -> [Int]
rowSpread m = fromMaybe [] $ do
  mn <- rowMin m
  mx <- rowMax m
  return [mn..mx]

rowMax :: (Map Position a) -> Maybe Int
rowMax m = do
  (((_, i), _), _) <- Map.maxViewWithKey m
  return i

rowMin :: (Map Position a) -> Maybe Int
rowMin m = do
  (((_, i), _), _) <- Map.minViewWithKey m
  return i

colMax :: (Map Position a) -> Maybe Int
colMax m = do
  (((i, _), _), _) <- Map.maxViewWithKey m
  return i

colMin :: (Map Position a) -> Maybe Int
colMin m = do
  (((i, _), _), _) <- Map.minViewWithKey m
  return i

filterByRow :: Map Position a -> Int -> Map Position a
filterByRow m r = Map.filterWithKey (\(_, i) ->  \_ -> r == i) m

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

rawEmptyBoard :: Map Position [String]
rawEmptyBoard = Map.fromList [((x,y), rawEmpty) | x <- size, y <- size]
  where
    size :: [Int]
    size = [0..8]
