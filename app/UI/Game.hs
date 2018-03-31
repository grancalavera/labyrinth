{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module UI.Game
  ( playGame
  )
where

import           Brick                          ( App(..)
                                                , AttrMap
                                                , BrickEvent(..)
                                                , EventM
                                                , Next
                                                , Widget
                                                , continue
                                                , halt
                                                , on
                                                )
import qualified Brick
import qualified Brick.Widgets.Center          as C
import           Control.Monad                  ( guard
                                                , void
                                                )
import           Lens.Micro                     ( (^.) )
import qualified Data.List.Extended            as L
import           Data.Map.Strict                ( Map )
import qualified Data.Map.Strict               as Map
import           Data.Maybe                     ( fromMaybe )
import           Data.Monoid                    ( (<>) )
import           Graphics.Vty                   ( Attr )
import qualified Graphics.Vty                  as V
import           Graphics.Vty.Input.Events      ( Modifier(..) )


import           Labyrinth
import qualified Labyrinth.Game                as Game
import qualified Labyrinth.Board               as Board
import qualified Labyrinth.Goal                as Goal
import qualified Labyrinth.Players             as Players
import qualified Labyrinth.Tile                as Tile

-- https://github.com/jtdaugherty/brick/blob/master/docs/guide.rst#resource-names
type Name = ()

playGame :: Game -> IO Name
playGame g = void $ Brick.defaultMain app g

app :: App Game e Name
app = App
  { appDraw         = drawUI
  , appHandleEvent  = handleEvent
  , appStartEvent   = return
  , appAttrMap      = const attributeMap
  , appChooseCursor = Brick.neverShowCursor
  }

drawUI :: Game -> [Widget Name]
drawUI g =
  [ C.vCenter $ C.hCenter $ Brick.vBox $ map (Brick.hBox . map snd)
                                             (Board.toRows board')
  ]
 where
  emptyWidgetBoard = emptyOf (fromRaw mempty)
  gates'           = Map.map (fromRaw . toRawGate) (g ^. Game.gates)
  tiles'           = Map.map fromTile (g ^. Game.tiles)
  board'           = tiles' <> gates' <> emptyWidgetBoard
  emptyOf          = emptyBoard (Game.rowSpread g) (Game.colSpread g)

handleEvent :: Game -> BrickEvent Name e -> EventM Name (Next Game)
handleEvent g@Game { _phase = Plan, ..} e = case e of
  VtyEvent (V.EvKey V.KRight []      ) -> continue $ Game.move East g
  VtyEvent (V.EvKey V.KLeft  []      ) -> continue $ Game.move West g
  VtyEvent (V.EvKey V.KUp    []      ) -> continue $ Game.move North g
  VtyEvent (V.EvKey V.KDown  []      ) -> continue $ Game.move South g
  VtyEvent (V.EvKey V.KRight [MShift]) -> continue $ Game.rotate g
  VtyEvent (V.EvKey V.KLeft  [MShift]) -> continue $ Game.rotate' g
  _ -> handleEventCommon g e
handleEvent g@Game { _phase = Walk, ..} e = case e of
  VtyEvent (V.EvKey V.KRight []) -> continue $ Game.walk East g
  VtyEvent (V.EvKey V.KLeft  []) -> continue $ Game.walk West g
  VtyEvent (V.EvKey V.KUp    []) -> continue $ Game.walk North g
  VtyEvent (V.EvKey V.KDown  []) -> continue $ Game.walk South g
  _                              -> handleEventCommon g e
handleEvent g e = handleEventCommon g e

handleEventCommon :: Game -> BrickEvent Name e -> EventM Name (Next Game)
handleEventCommon g (VtyEvent (V.EvKey (V.KChar 'q') [])) = halt g
handleEventCommon g (VtyEvent (V.EvKey V.KEsc [])) = halt g
handleEventCommon g (VtyEvent (V.EvKey V.KEnter [])) = continue $ Game.done g
handleEventCommon g (VtyEvent (V.EvKey (V.KChar ' ') [])) =
  continue $ Game.done g
handleEventCommon g _ = continue g

toRawGate :: Gate -> RawCell
toRawGate (Gate _ False) = Empty
toRawGate (Gate d _    ) = Cell $ case d of
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
toRawTile t = mempty <> toRawFound t <> toRawTreasure t <> toRawTerrain t

toRawTreasure :: Tile -> RawCell
toRawTreasure t = fromMaybe mempty $ do
  (Goal t' _) <- t ^. Tile.goal
  c           <- Map.lookup t' treasureMap
  return
    $  Cell
    $  "         " ++
       "         " ++
       "    "  ++ [c]  ++ "    " ++
       "         "

treasureMap :: Map Treasure Char
treasureMap = Map.fromList $ zip Goal.treasures ['A' ..]

toRawFound :: Tile -> RawCell
toRawFound t = fromMaybe mempty $ do
  (Goal _ isFound) <- t ^. Tile.goal
  guard isFound
  return $ Cell $ "         " ++
                  "         " ++
                  "    ✓    " ++
                  "         "

toRawTerrain :: Tile -> RawCell
toRawTerrain t = Cell $ case (t ^. Tile.terrain, t ^. Tile.direction) of
  (Path  , North) -> " │     │ " ++
                     " │     │ " ++
                     " │     │ " ++
                     " │     │ "
  (Path  , West ) -> "─────────" ++
                     "         " ++
                     "         " ++
                     "─────────"
  (Path  , South) -> " │     │ " ++
                     " │     │ " ++
                     " │     │ " ++
                     " │     │ "
  (Path  , East ) -> "─────────" ++
                     "         " ++
                     "         " ++
                     "─────────"
  (Corner, North) -> "─┘     │ " ++
                     "       │ " ++
                     "       │ " ++
                     "───────┘ "
  (Corner, West ) -> "───────┐ " ++
                     "       │ " ++
                     "       │ " ++
                     "─┐     │ "
  (Corner, South) -> " ┌───────" ++
                     " │       " ++
                     " │       " ++
                     " │     ┌─"
  (Corner, East ) -> " │     └─" ++
                     " │       " ++
                     " │       " ++
                     " └───────"
  (Fork  , North) -> "─┘     └─" ++
                     "         " ++
                     "         " ++
                     "─────────"
  (Fork  , West ) -> "─┘     │ " ++
                     "       │ " ++
                     "       │ " ++
                     "─┐     │ "
  (Fork  , South) -> "─────────" ++
                     "         " ++
                     "         " ++
                     "─┐     ┌─"
  (Fork  , East ) -> " │     └─" ++
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

widget2p :: String -> Widget Name
widget2p = toWidget 9

widget3p :: String -> Widget Name
widget3p = toWidget 3

widget4p :: String -> Widget Name
widget4p = toWidget 9

toWidget :: Int -> String -> Widget Name
toWidget cols raw = Brick.str $ L.intercalate "\n" $ L.splitEvery cols raw

fromRaw :: RawCell -> Widget Name
fromRaw = toWidget 9 . extract

extract :: RawCell -> String
extract Empty     = replicate (9 * 4) ' '
extract (Cell xs) = xs

emptyBoard :: [Int] -> [Int] -> a -> Map Position a
emptyBoard rows cols empty =
  Map.fromList [ ((x, y), empty) | x <- rows, y <- cols ]

attributeMap :: AttrMap
attributeMap = Brick.attrMap
  defaultAttr
  [ ("default", defaultAttr)
  , ("Yellow" , V.white `on` V.yellow)
  , ("Blue"   , V.white `on` V.blue)
  , ("Green"  , V.white `on` V.green)
  , ("Red"    , V.white `on` V.red)
  ]

defaultAttr :: Attr
defaultAttr = V.white `on` V.black

fromTile :: Tile -> Widget Name
fromTile t = case t ^. Tile.players of
  [p1] -> Brick.withAttr (attr p1) $ fromRaw rawTile
  [p1, p2] ->
    let (p1', p2') = twoPlayers $ extract rawTile
    in  Brick.vBox
          [ Brick.withAttr (attr p1) $ widget2p p1'
          , Brick.withAttr (attr p2) $ widget2p p2'
          ]
  [p1, p2, p3] ->
    let (p1', p2', p3') = threePlayers $ extract rawTile
    in  Brick.hBox
          [ Brick.withAttr (attr p1) $ widget3p p1'
          , Brick.withAttr (attr p2) $ widget3p p2'
          , Brick.withAttr (attr p3) $ widget3p p3'
          ]
  [p1, p2, p3, p4] ->
    let (p1', p2', p3', p4') = fourPlayers $ extract rawTile
    in  Brick.vBox
          [ Brick.withAttr (attr p1) $ widget4p p1'
          , Brick.withAttr (attr p2) $ widget4p p2'
          , Brick.withAttr (attr p3) $ widget4p p3'
          , Brick.withAttr (attr p4) $ widget4p p4'
          ]
  _ -> Brick.withAttr "default" $ fromRaw rawTile
 where
  rawTile = toRawTile t
  attr p = Brick.attrName $ show (p ^. Players.color)

twoPlayers :: String -> (String, String)
twoPlayers xs =
  foldl
      (\(p1, p2) (i, x) -> case i `div` w of
        0 -> (p1 ++ [x], p2)
        1 -> (p1, p2 ++ [x])
        _ -> (p1, p2)
      )
      ([], [])
    $ zip [0 ..] xs
  where w = length xs `div` 2

threePlayers :: String -> (String, String, String)
threePlayers xs =
  foldl
      (\(p1, p2, p3) (i, x) -> case group i of
        0 -> (p1 ++ [x], p2, p3)
        1 -> (p1, p2 ++ [x], p3)
        2 -> (p1, p2, p3 ++ [x])
        _ -> (p1, p2, p3)
      )
      ([], [], [])
    $ zip [0 ..] xs
 where
  cols  = 9
  count = 3
  group :: Int -> Int
  group i = ((i `mod` cols) `div` count) `mod` count

fourPlayers :: String -> (String, String, String, String)
fourPlayers xs =
  foldl
      (\(p1, p2, p3, p4) (i, x) -> case i `div` w of
        0 -> (p1 ++ [x], p2, p3, p4)
        1 -> (p1, p2 ++ [x], p3, p4)
        2 -> (p1, p2, p3 ++ [x], p4)
        3 -> (p1, p2, p3, p4 ++ [x])
        _ -> (p1, p2, p3, p4)
      )
      ([], [], [], [])
    $ zip [0 ..] xs
  where w = length xs `div` 4
