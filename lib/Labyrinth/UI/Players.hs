{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

module Labyrinth.UI.Players
    (addPlayers) where

import           Data.Maybe           (fromJust, fromMaybe)
import qualified Data.Text            as T
import           Data.Text            (Text)
import           Data.List            (isInfixOf)
import           Lens.Micro           ((^.), Lens')
import           Lens.Micro.TH
import           Data.Monoid          ((<>))
import qualified Graphics.Vty         as V
import           Brick
import           Brick.Forms          ( Form
                                      , newForm
                                      , formState
                                      , invalidFields
                                      , formFocus
                                      , renderForm
                                      , handleFormEvent
                                      , focusedFormInputAttr
                                      , invalidFormInputAttr
                                      , editTextField
                                      , checkboxField
                                      , setFieldValid
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
  [ (E.editAttr,           V.white `on` V.brightBlack)
  , (E.editFocusedAttr,    V.brightBlack `on` V.brightYellow)
  , (invalidFormInputAttr, V.white `on` V.brightBlack)
  , (focusedFormInputAttr, V.black `on` V.white)
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
  VtyEvent (V.EvKey V.KEsc [])    -> halt s
  VtyEvent (V.EvKey V.KEnter [])  -> halt s
  _                               -> do
    s' <- handleFormEvent e s
    continue $ ( (setFieldValid (valid $ (formState s') ^. p1) P1Field)
               . (setFieldValid (valid $ (formState s') ^. p2) P2Field)
               . (setFieldValid (valid $ (formState s') ^. p3) P3Field)
               . (setFieldValid (valid $ (formState s') ^. p4) P4Field)
               ) s'

addPlayers :: IO Players
addPlayers = addPlayers' initialState
  where
    initialState = PlayersInfo { _p1 = ""
                               , _p2 = ""
                               , _p3 = ""
                               , _p4 = ""
                               }

addPlayers' :: PlayersInfo -> IO Players
addPlayers' initialState = do
  f <- Brick.defaultMain app $ mkForm initialState
  let st = formState f
      ps = map (\(c,p) -> Player c p)
            $ zip Players.colors
            $ filter valid
            $ map (st ^.) [p1, p2, p3, p4]

  print ps
  -- return $ fromJust $ fromMaybe (addPlayers' s) $ Players.fromList m


  return $ fromJust $ Players.fromList
    [ Player Yellow "Yellow Player"
    , Player Blue "Blue Player"
    , Player Green "Green Player"
    , Player Red "Red Player"
    ]

valid :: Text -> Bool
valid = (/= "")
