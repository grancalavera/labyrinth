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
import qualified Brick.Widgets.Border          as B
import qualified Brick.Widgets.Center          as C
import           Control.Monad                  ( void )
import           Lens.Micro                     ( (^.) )
import qualified Data.List.Extended            as L
import           Data.Map.Strict                ( Map )
import qualified Data.Map.Strict               as Map
import           Data.Maybe                     ( fromMaybe )
import           Data.Monoid                    ( (<>) )
import qualified Graphics.Vty                  as V
import           Graphics.Vty.Input.Events      ( Modifier(..) )
import           Linear.V2                      ( V2(..)
                                                , _x
                                                )
import qualified Data.Text                     as T
import           Labyrinth
import qualified Labyrinth.Game                as Game
import qualified Labyrinth.Treasure            as Treasure
import qualified Labyrinth.Players             as Players
import qualified Labyrinth.Tile                as Tile
import qualified UI.Graphics                   as Graphics

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
drawUI g = [C.vCenter $ C.hCenter $ Brick.vBox [player g, board g]]

player :: Game -> Widget Name
player g =
  Brick.withAttr c
    $  Brick.vLimit 1
    $  Brick.hLimit ((length (g ^. Game.cols) * Graphics.width) + 2)
    $  C.vCenter
    $  C.hCenter
    $  Brick.str
    $  T.unpack
    $  Game.player g
    ^. Players.name
  where c = Brick.attrName $ show $ g ^. Game.playing

board :: Game -> Widget Name
board g = Brick.padTop (Brick.Pad 1) $ B.border $ Brick.vBox $ map
  (Brick.hBox . map snd)
  (toRows 0 (Game.lastRow g) board')
 where
  emptyWidgetBoard = emptyOf (fromRaw mempty)
  gates'           = Map.map (fromRaw . toRawGate) (g ^. Game.gates)
  tiles'           = Map.map fromTile (g ^. Game.tiles)
  board'           = tiles' <> gates' <> emptyWidgetBoard
  emptyOf          = emptyBoard (g ^. Game.rows) (g ^. Game.cols)

handleEvent :: Game -> BrickEvent Name e -> EventM Name (Next Game)
handleEvent g@Game { _phase = Plan, ..} e = case e of
  VtyEvent (V.EvKey V.KRight []      ) -> continue $ Game.moveTile East g
  VtyEvent (V.EvKey V.KLeft  []      ) -> continue $ Game.moveTile West g
  VtyEvent (V.EvKey V.KUp    []      ) -> continue $ Game.moveTile North g
  VtyEvent (V.EvKey V.KDown  []      ) -> continue $ Game.moveTile South g
  VtyEvent (V.EvKey V.KRight [MShift]) -> continue $ Game.rotateTile g
  VtyEvent (V.EvKey V.KLeft  [MShift]) -> continue $ Game.rotateTile' g
  _ -> handleEventCommon g e
handleEvent g@Game { _phase = Walk, ..} e = case e of
  VtyEvent (V.EvKey V.KRight []) -> continue $ Game.moveToken East g
  VtyEvent (V.EvKey V.KLeft  []) -> continue $ Game.moveToken West g
  VtyEvent (V.EvKey V.KUp    []) -> continue $ Game.moveToken North g
  VtyEvent (V.EvKey V.KDown  []) -> continue $ Game.moveToken South g
  _                              -> handleEventCommon g e
handleEvent g e = handleEventCommon g e

handleEventCommon :: Game -> BrickEvent Name e -> EventM Name (Next Game)
handleEventCommon g (VtyEvent (V.EvKey (V.KChar 'q') [])) = halt g
handleEventCommon g (VtyEvent (V.EvKey V.KEsc        [])) = halt g
handleEventCommon g (VtyEvent (V.EvKey V.KEnter      [])) = continue $ Game.done g
handleEventCommon g (VtyEvent (V.EvKey (V.KChar ' ') [])) = continue $ Game.done g
handleEventCommon g _ = continue g

toRawGate :: Gate -> RawCell
toRawGate (Gate _ False) = Empty
toRawGate (Gate d _    ) = Cell $ Graphics.gate d

toRawTile :: Tile -> RawCell
toRawTile t = mempty <> toRawTreasure t <> toRawTerrain t

toRawTreasure :: Tile -> RawCell
toRawTreasure t = fromMaybe mempty $ do
  t' <- t ^. Tile.treasure
  c  <- Map.lookup t' treasureMap
  return $ Cell $ Graphics.treasure c

treasureMap :: Map Treasure Char
treasureMap = Map.fromList $ zip Treasure.treasures ['A' ..]

toRawTerrain :: Tile -> RawCell
toRawTerrain t = Cell $ Graphics.tile (t ^. Tile.terrain) (t ^. Tile.direction)

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
widget2p = toWidget Graphics.width

widget3p :: String -> Widget Name
widget3p = toWidget 3

widget4p :: String -> Widget Name
widget4p = toWidget Graphics.width

toWidget :: Int -> String -> Widget Name
toWidget cols raw = Brick.str $ L.intercalate "\n" $ L.splitEvery cols raw

fromRaw :: RawCell -> Widget Name
fromRaw = toWidget Graphics.width . extract

extract :: RawCell -> String
extract Empty     = replicate (Graphics.width * Graphics.height) ' '
extract (Cell xs) = xs

emptyBoard :: [Int] -> [Int] -> a -> Map Position a
emptyBoard rows cols empty = Map.fromList [ (V2 x y, empty) | x <- rows, y <- cols ]

attributeMap :: AttrMap
attributeMap = Brick.attrMap
  V.defAttr
  [ ("Yellow", V.black `on` V.yellow)
  , ("Blue"  , V.black `on` V.blue)
  , ("Green" , V.black `on` V.green)
  , ("Red"   , V.black `on` V.red)
  ]

fromTile :: Tile -> Widget Name
fromTile t = case Tile.tokenList t of
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
  attr c = Brick.attrName $ show c

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
  group i = ((i `mod` Graphics.width) `div` count) `mod` count
  count = 3

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

toRows :: Int -> Int -> Map Position a -> [[(Position, a)]]
toRows mn mx m = map (Map.toList . (`getRow` m)) [mn .. mx]

getRow :: Int -> Map Position a -> Map Position a
getRow r = Map.filterWithKey (\p _ -> p ^. _x == r)
