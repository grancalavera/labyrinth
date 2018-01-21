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
import           Labyrinth.Cell         (Cell(..))
import           Labyrinth.Gate         (Gate)
import           Labyrinth.Tile         ( Tile(..)
                                        , Terrain(..)
                                        )
import qualified Labyrinth.Board        as Board
import           Labyrinth.Board        (Board, Position)
import qualified Labyrinth.Game         as Game
import           Labyrinth.Game         (Game, gates)--, tiles)

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
  [ Brick.vBox $ map toWidgetRow (toRows board')
  ]
  where
    gates' :: Map Position [String]
    gates' = Map.mapWithKey rawFromPosAndGate (g ^. gates)
    board' :: Map Position [String]
    board' = Map.union gates' rawEmptyBoard

toWidgetRow :: [(Position, [String])] -> Widget ()
toWidgetRow r = Brick.hBox $ map (fromRaw . snd) r
-- toWidgetRow r = Brick.hBox $ map (\ _ -> Brick.str " cell ") r

toRows :: Map Position a -> [[(Position, a)]]
toRows m = map (Map.toList . (filterByRow m)) (rowSpread m)

rowSpread :: (Map Position a) -> [Int]
rowSpread m = fromMaybe [] $ do
  mn <- rowMin m
  mx <- rowMax m
  return [mn..mx]

rowMax :: (Map Position a) -> Maybe Int
rowMax m = do
  (((_, r), _), _) <- Map.maxViewWithKey m
  return r

rowMin :: (Map Position a) -> Maybe Int
rowMin m = do
  (((_, r), _), _) <- Map.minViewWithKey m
  return r

filterByRow :: Map Position a -> Int -> Map Position a
filterByRow m r = Map.filterWithKey (\(_, i) ->  \_ -> r == i) m

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

rawFromPosAndGate :: Position -> Gate -> [String]
rawFromPosAndGate _ _ = ["       ",
                         "   ▲   ",
                         "       "]

-- type WMap = Map Position (Widget ())

-- toGateMap :: Board Gate -> WMap
-- toGateMap = toWidgetMap widgetFromGate

-- toTileMap :: Board Tile -> WMap
-- toTileMap = toWidgetMap widgetFromTile

-- toWidgetMap :: (Cell a -> Widget()) -> Board a -> WMap
-- toWidgetMap f b = Map.map f (Board.toMap b)

-- widgetFromGate :: Cell Gate -> Widget ()
-- widgetFromGate Empty = empty
-- widgetFromGate (Cell d _) = fromRaw $ case d of
--   North -> ["       ",
--             "   ▲   ",
--             "       "]
--   West  -> ["       ",
--             "  ◄    ",
--             "       "]
--   South -> ["       ",
--             "   ▼   ",
--             "       "]
--   East  -> ["       ",
--             "    ►  ",
--             "       "]

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
