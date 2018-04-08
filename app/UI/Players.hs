{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module UI.Players
  ( addPlayers
  )
where

import           Brick
import           Brick.Focus                    ( focusRingCursor )
import           Brick.Forms                    ( Form
                                                , editTextField
                                                , focusedFormInputAttr
                                                , formFocus
                                                , formState
                                                , handleFormEvent
                                                , invalidFormInputAttr
                                                , newForm
                                                , renderForm
                                                , (@@=)
                                                )
import qualified Brick.Widgets.Border          as B
import qualified Brick.Widgets.Center          as C
import qualified Brick.Widgets.Edit            as E
import qualified Data.Text                     as T
import qualified Graphics.Vty                  as V
import           Lens.Micro                     ( (^.) )
import           Lens.Micro.TH

import           Labyrinth.Players              ( Player(..)
                                                , Players
                                                )
import qualified Labyrinth.Players             as Players

data PlayersInfo = PlayersInfo
  { _p1 :: T.Text
  , _p2 :: T.Text
  , _p3 :: T.Text
  , _p4 :: T.Text
  } deriving (Show)
makeLenses ''PlayersInfo

data Name = P1Field
          | P2Field
          | P3Field
          | P4Field
          deriving (Eq, Ord, Show)

addPlayers :: IO Players
addPlayers = addPlayers' PlayersInfo {_p1 = "a", _p2 = "b", _p3 = "c", _p4 = "d"}

addPlayers' :: PlayersInfo -> IO Players
addPlayers' initialState = do
  f <- Brick.defaultMain app $ mkForm initialState
  let st = formState f
      ps =
        map (\(c, n) -> (c, Player {_name = n}))
          $ filter ((/= "") . snd)
          $ map (\(c, l) -> (c, st ^. l))
          $ zip Players.colors [p1, p2, p3, p4]

  case Players.fromList ps of
    (Just ps') -> return ps'
    Nothing    -> addPlayers' st

mkForm :: PlayersInfo -> Form PlayersInfo e Name
mkForm =
  let chip a w =
        padBottom (Pad 1)
          $   padRight (Pad 1) (withAttr a $ vLimit 1 $ hLimit 3 $ fill ' ')
          <+> w
  in  newForm
        [ chip "yp" @@= editTextField p1 P1Field (Just 1)
        , chip "rp" @@= editTextField p2 P2Field (Just 1)
        , chip "bp" @@= editTextField p3 P3Field (Just 1)
        , chip "gp" @@= editTextField p4 P4Field (Just 1)
        ]

theMap :: AttrMap
theMap = attrMap
  V.defAttr
  [ (E.editFocusedAttr   , V.brightBlack `on` V.white)
  , (invalidFormInputAttr, V.white `on` V.brightBlack)
  , (focusedFormInputAttr, V.black `on` V.white)
  , ("yp"                , V.white `on` V.yellow)
  , ("rp"                , V.white `on` V.red)
  , ("bp"                , V.white `on` V.blue)
  , ("gp"                , V.white `on` V.green)
  ]

draw :: Form PlayersInfo e Name -> [Widget Name]
draw f = [C.vCenter $ C.hCenter form]
  where form = B.border $ padTop (Pad 1) $ hLimit 50 $ renderForm f

app :: App (Form PlayersInfo e Name) e Name
app = App
  { appDraw         = draw
  , appHandleEvent  = handleEvent
  , appChooseCursor = focusRingCursor formFocus
  , appStartEvent   = return
  , appAttrMap      = const theMap
  }

handleEvent
  :: Form PlayersInfo e Name
  -> BrickEvent Name e
  -> EventM Name (Next (Form PlayersInfo e Name))
handleEvent s e = case e of
  VtyEvent (V.EvKey V.KEsc   []) -> halt s
  VtyEvent (V.EvKey V.KEnter []) -> halt s
  _                              -> handleFormEvent e s >>= continue
