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
  let label s w = padBottom (Pad 1)
                    $ (vLimit 1 $ hLimit 15 $ str s <+> fill ' ') <+> w
  in newForm
    [ label "Player 1" @@= editTextField p1 P1Field (Just 16)
    , label "Player 2" @@= editTextField p1 P2Field (Just 16)
    , label "Player 3" @@= editTextField p1 P3Field (Just 16)
    , label "Player 4" @@= editTextField p1 P4Field (Just 16)
    ]

theMap :: AttrMap
theMap = attrMap V.defAttr
  [ (E.editAttr,           V.white `on` V.black)
  , (E.editFocusedAttr,    V.black `on` V.brightBlack)
  , (focusedFormInputAttr, V.black `on` V.brightBlack)
  ]

draw :: Form PlayersInfo e Name -> [Widget Name]
draw f = [C.vCenter $ C.hCenter form]
  where
    form = B.border $ padTop (Pad 1) $ hLimit 15 $ renderForm f

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
  VtyEvent (V.EvKey (V.KChar 'q') []) -> halt s
  _                            -> continue s

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
