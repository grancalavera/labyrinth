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

import qualified Labyrinth.Players    as Players
import           Labyrinth.Players    (Players, Player(..), Color(..))

data PlayersInfo = PlayersInfo
  { _p1 :: T.Text
  , _p2 :: T.Text
  , _p3 :: T.Text
  , _P4 :: T.Text
  } deriving (Show)
makeLenses ''PlayersInfo

data Name = P1Field
          | P2Field
          | P3Field
          | P4Field
          deriving (Eq, Ord, Show)

addPlayers :: IO Players
addPlayers = return $ fromJust $ Players.fromList
  [ Player Yellow "Yellow Player"
  , Player Blue "Blue Player"
  , Player Green "Green Player"
  , Player Red "Red Player"
  ]

mkForm :: PlayersInfo -> Form PlayersInfo e Name
mkForm =
  let label s w = padBottom (Pad 1) $ (vLimit 1 $ hLimit 15 $ str s <+> fill ' ') <+> w
  in newForm
    [ label "Player 1" @@= editTextField p1 P1Field (Just 16)
    , label "Player 2" @@= editTextField p1 P2Field (Just 16)
    , label "Player 3" @@= editTextField p1 P3Field (Just 16)
    , label "Player 4" @@= editTextField p1 P4Field (Just 16)
    ]


{-
mkForm :: UserInfo -> Form UserInfo e Name
mkForm =
    let label s w = padBottom (Pad 1)
      $   (vLimit 1 $ hLimit 15 $ str s <+> fill ' ')
      <+> w
    in newForm
      [ label "Name"          @@= editTextField name NameField (Just 1)
      , label "Address"       @@= B.borderWithLabel (str "Mailing")
                                @@= editTextField address AddressField (Just 3)
      , label "Age"           @@= editShowableField age AgeField
      , label "Password"      @@= editPasswordField password PasswordField
      , label "Dominant hand" @@= radioField handed
                                [ (LeftHanded, LeftHandField, "Left")
                                , (RightHanded, RightHandField, "Right")
                                , (Ambidextrous, AmbiField, "Both")
                                ]
      , label ""              @@= checkboxField ridesBike BikeField "etc"
               ]
-}
