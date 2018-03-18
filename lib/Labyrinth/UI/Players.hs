{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

module Labyrinth.UI.Players
    (addPlayers) where

import           Data.Maybe          (fromJust)
import qualified Data.Text           as T
import           Lens.Micro          ((^.))
import           Lens.Micro.TH
import           Data.Monoid         ((<>))

import qualified Graphics.Vty         as V
import           Brick
import           Brick.Forms          ( Form
                                      , newForm
                                      , formState
                                      , formFocus
                                      , renderForm
                                      , handleFormEvent
                                      , focusedFormInputAttr
                                      , editTextField
                                      , (@@=)
                                      )
import           Brick.Focus          ( focusGetCurrent
                                      , focusRingCursor
                                      )
import qualified Brick.Widgets.Edit   as E
import qualified Brick.Widgets.Center as C
import qualified Brick.Widgets.Border as B

import qualified Labyrinth.Players    as Players
import           Labyrinth.Players    (Players, Player(..), Color(..))

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

mkForm :: PlayersInfo -> Form PlayersInfo e Name
mkForm =
  let chip a w = padBottom (Pad 1) $ ( padRight (Pad 1)
                                     $ withAttr a
                                     $ vLimit 1
                                     $ hLimit 3
                                     $ fill ' '
                                     )
                                   <+> w

  in newForm
    [ chip "yp" @@= editTextField p1 P1Field (Just 1)
    , chip "rp" @@= editTextField p2 P2Field (Just 1)
    , chip "bp" @@= editTextField p3 P3Field (Just 1)
    , chip "gp" @@= editTextField p4 P4Field (Just 1)
    ]

theMap :: AttrMap
theMap = attrMap V.defAttr
  [ (E.editAttr,           V.white `on` V.black)
  , (E.editFocusedAttr,    V.white `on` V.brightBlack)
  , (focusedFormInputAttr, V.white `on` V.brightBlack)
  , ("yp", V.white `on` V.yellow)
  , ("rp", V.white `on` V.red)
  , ("bp", V.white `on` V.blue)
  , ("gp", V.white `on` V.green)
  ]

draw :: Form PlayersInfo e Name -> [Widget Name]
draw f = [C.vCenter $ C.hCenter form]
  where
    form = B.border $ padTop (Pad 1) $ hLimit 50 $ renderForm f

app :: App (Form PlayersInfo e Name) e Name
app = App
  { appDraw         = draw
  , appHandleEvent  = handleEvent
  , appChooseCursor = focusRingCursor formFocus
  , appStartEvent   = return
  , appAttrMap      = const theMap
  }

handleEvent :: Form PlayersInfo e Name
  -> BrickEvent Name e
  -> EventM Name (Next (Form PlayersInfo e Name))
handleEvent s e = case e of
  VtyEvent (V.EvKey V.KEsc [])        -> halt s
  _                                   -> do
    s' <- handleFormEvent e s
    continue s'

addPlayers :: IO Players
addPlayers = do
  let initialState = PlayersInfo { _p1 = ""
                                 , _p2 = ""
                                 , _p3 = ""
                                 , _p4 = ""
                                 }
  Brick.defaultMain app $ mkForm initialState

  return $ fromJust $ Players.fromList
    [ Player Yellow "Yellow Player"
    , Player Blue "Blue Player"
    , Player Green "Green Player"
    , Player Red "Red Player"
    ]
