{-# LANGUAGE OverloadedStrings #-}

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
                                        , AttrMap
                                        , on
                                        )
import qualified Graphics.Vty           as V
import           Graphics.Vty           (Attr)
import           Data.List              (intercalate)
import qualified Labyrinth              as Labyrinth
import           Labyrinth              (Position)
import           Labyrinth.Players      (Players, Color(..), color)
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
          , appAttrMap      = const attributeMap
          , appChooseCursor = Brick.neverShowCursor
          }

drawUI :: Game -> [Widget ()]
drawUI g =
  [ C.vCenter $ C.hCenter
              $ Brick.vBox
              $ map (Brick.hBox . (map snd)) (Labyrinth.toRows board')
  ]
  where

    emptyWidgetBoard = emptyOf (fromRaw mempty)
    gates' = Map.map (fromRaw . toRawGate) (g ^. gates)
    tiles' = Map.map (\t -> 
      let rt = toRawTile t 
          t' = fromRaw rt
      in withAttr t t'
      ) (g ^. tiles)

    board' = tiles' <> gates' <> emptyWidgetBoard
    emptyOf = emptyBoard (g ^. rowSpread) (g ^. colSpread)

toRawGate :: Gate -> RawCell
toRawGate (Gate d _) = Cell $ case d of
  North -> "         " ++
           "   ▲ ▲   " ++
           "         " ++
           "         "
  West  -> "         " ++
           "   ◄     " ++
           "   ◄     " ++
           "         "
  South -> "         " ++
           "         " ++
           "   ▼ ▼   " ++
           "         "
  East  -> "         " ++
           "     ►   " ++
           "     ►   " ++
           "         "

toRawTile :: Tile -> RawCell
toRawTile t =  mempty
            <> toRawFound    t
            <> toRawTreasure t
            <> toRawTerrain  t

toRawTreasure :: Tile -> RawCell
toRawTreasure t = fromMaybe mempty $ do
  (Goal t' _) <- t ^. goal
  c           <- Map.lookup t' treasureMap
  return $ Cell $ "         " ++
                  "         " ++
                  "    " ++ [c] ++ "    " ++
                  "         " 

treasureMap :: Map Treasure Char
treasureMap = Map.fromList $ zip Goal.treasures ['A'..]

toRawFound :: Tile -> RawCell
toRawFound t = fromMaybe mempty $ do
  (Goal _ isFound) <- t ^. goal
  guard isFound
  return $ Cell $ "         " ++
                  "         " ++
                  "    ✓    " ++
                  "         " 

toRawTerrain :: Tile -> RawCell
toRawTerrain t = Cell $ case (t ^. terrain, t ^. direction) of
  (Path, North)   -> " │     │ " ++
                     " │     │ " ++
                     " │     │ " ++
                     " │     │ "
  (Path, West)    -> "─────────" ++
                     "         " ++
                     "         " ++
                     "─────────"
  (Path, South)   -> " │     │ " ++
                     " │     │ " ++
                     " │     │ " ++
                     " │     │ "
  (Path, East)    -> "─────────" ++
                     "         " ++
                     "         " ++
                     "─────────"
  (Corner, North) -> "─┘     │ " ++
                     "       │ " ++
                     "       │ " ++
                     "───────┘ "
  (Corner, West)  -> "───────┐ " ++
                     "       │ " ++
                     "       │ " ++
                     "─┐     │ "
  (Corner, South) -> " ┌───────" ++
                     " │       " ++
                     " │       " ++
                     " │     ┌─"
  (Corner, East)  -> " │     └─" ++
                     " │       " ++
                     " │       " ++
                     " └───────"
  (Fork, North)   -> "─┘     └─" ++
                     "         " ++
                     "         " ++
                     "─────────"
  (Fork, West)    -> "─┘     │ " ++
                     "       │ " ++
                     "       │ " ++
                     "─┐     │ "
  (Fork, South)   -> "─────────" ++
                     "         " ++
                     "         " ++
                     "─┐     ┌─"
  (Fork, East)    -> " │     └─" ++
                     " │       " ++
                     " │       " ++
                     " │     ┌─"

data RawCell = Cell String | Empty

instance Monoid RawCell where
  mempty = Empty
  Empty     `mappend` Empty  = Empty
  Empty     `mappend` Cell x = Cell x
  Cell x    `mappend` Empty  = Cell x
  Cell x    `mappend` Cell y = Cell (zipWith choose x y)

choose :: Char -> Char -> Char
choose ' ' c   = c
choose c   ' ' = c
choose c   _   = c

fromRaw :: RawCell -> Widget ()
fromRaw = fromRawWithSize 4 9

fromRawWithSize :: Int -> Int -> RawCell -> Widget ()
fromRawWithSize rows cols raw = Brick.str $ intercalate "\n" $ case raw of 
  Empty   -> replicate rows $ replicate cols ' '
  Cell xs -> Labyrinth.splitEvery cols xs

emptyBoard :: [Int] -> [Int] -> a -> Map Position a
emptyBoard rows cols empty = Map.fromList [((x,y), empty) | x <- rows, y <- cols]

attributeMap :: AttrMap
attributeMap = Brick.attrMap defaultAttr
  [ ("default",       defaultAttr)
  , ("yellowPlayer",  V.black `on` V.yellow)
  , ("bluePlayer",    V.black `on` V.blue)
  , ("greenPlayer",   V.black `on` V.green)
  , ("redPlayer",     V.black `on` V.red)
  , ("funky",         V.white `on` V.brightRed)
  ]

defaultAttr :: Attr
defaultAttr = V.white `on` V.black

withAttr :: Tile -> Widget () -> Widget ()
withAttr _ = Brick.withAttr "funky"
