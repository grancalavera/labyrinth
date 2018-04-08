{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TemplateHaskell   #-}

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
import           Lens.Micro                     ( (^.)
                                                , (&)
                                                , (.~)
                                                )
import           Lens.Micro.TH                  ( makeLenses )
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
import qualified Labyrinth.Game                as G
import qualified Labyrinth.Treasure            as Treasure
import qualified Labyrinth.Players             as Players
import qualified Labyrinth.Tile                as Tile
import qualified UI.Graphics                   as Graphics

-- https://github.com/jtdaugherty/brick/blob/master/docs/guide.rst#resource-names
type Name = ()
data UI = UI {
  _game :: Game
, _hint :: String
} deriving (Show)
makeLenses ''UI

playGame :: Game -> IO Name
playGame g = void $ Brick.defaultMain app UI {_game = g, _hint = ""}

app :: App UI e Name
app = App
  { appDraw         = drawUI
  , appHandleEvent  = handleEvent
  , appStartEvent   = return
  , appAttrMap      = const attributeMap
  , appChooseCursor = Brick.neverShowCursor
  }

drawUI :: UI -> [Widget Name]
drawUI g = [C.vCenter $ C.hCenter $ Brick.vBox [player g, board g]]

player :: UI -> Widget Name
player gui =
  Brick.withAttr c
    $ Brick.vLimit 1
    $ Brick.hLimit ((length (g ^. G.cols) * Graphics.width) + 2)
    $ C.vCenter
    $ C.hCenter
    $ Brick.str label
 where
  g     = gui ^. game
  c     = Brick.attrName $ show $ g ^. G.playing
  label = "Playing: " ++ T.unpack (G.player g ^. Players.name)

board :: UI -> Widget Name
board gui = Brick.padTop (Brick.Pad 1) $ B.border $ Brick.vBox $ map
  (Brick.hBox . map snd)
  (toRows 0 (G.lastRow g) board')
 where
  g                = gui ^. game
  emptyWidgetBoard = emptyOf (fromRaw mempty)
  gates'           = Map.map (fromRaw . toRawGate) (g ^. G.gates)
  tiles'           = Map.map fromTile (g ^. G.tiles)
  board'           = tiles' <> gates' <> emptyWidgetBoard
  emptyOf          = emptyBoard (g ^. G.rows) (g ^. G.cols)

handleEvent :: UI -> BrickEvent Name e -> EventM Name (Next UI)
handleEvent ui e = case ui ^. game of
  Game { _phase = Plan, ..} -> case e of
    VtyEvent (V.EvKey V.KRight []      ) -> continue $ inGame (G.moveTile East) ui
    VtyEvent (V.EvKey V.KLeft  []      ) -> continue $ inGame (G.moveTile West) ui
    VtyEvent (V.EvKey V.KUp    []      ) -> continue $ inGame (G.moveTile North) ui
    VtyEvent (V.EvKey V.KDown  []      ) -> continue $ inGame (G.moveTile South) ui
    VtyEvent (V.EvKey V.KRight [MShift]) -> continue $ inGame G.rotateTile ui
    VtyEvent (V.EvKey V.KLeft  [MShift]) -> continue $ inGame G.rotateTile' ui
    _ -> handleCommon ui e
  Game { _phase = Walk, ..} -> case e of
    VtyEvent (V.EvKey V.KRight []) -> continue $ inGame (G.moveToken East) ui
    VtyEvent (V.EvKey V.KLeft  []) -> continue $ inGame (G.moveToken West) ui
    VtyEvent (V.EvKey V.KUp    []) -> continue $ inGame (G.moveToken North) ui
    VtyEvent (V.EvKey V.KDown  []) -> continue $ inGame (G.moveToken South) ui
    _                              -> handleCommon ui e
  Game { _phase = Over, ..} -> handleCommon ui e

handleCommon :: UI -> BrickEvent Name e -> EventM Name (Next UI)
handleCommon ui (VtyEvent (V.EvKey (V.KChar 'q') [])) = halt ui
handleCommon ui (VtyEvent (V.EvKey V.KEsc        [])) = halt ui
handleCommon ui (VtyEvent (V.EvKey V.KEnter      [])) = continue $ inGame G.done ui
handleCommon ui (VtyEvent (V.EvKey (V.KChar ' ') [])) = continue $ inGame G.done ui
handleCommon ui _ = continue ui

inGame :: (Game -> Game) -> UI -> UI
inGame f ui = ui & game .~ f (ui ^. game)

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
