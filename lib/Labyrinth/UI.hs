module Labyrinth.UI
  ( Screen(..)
  , Modal(..)
  , Global
  , SplashScreen
  , RegistrationScreen
  , Name(..)
  , attributeMap
  )
where

import           Brick
import qualified Brick.Widgets.Edit            as E
import           Brick.Forms                    ( focusedFormInputAttr )
import qualified Graphics.Vty                  as V

import           Labyrinth.UI.Internal
import           Labyrinth.UI.Global
import           Labyrinth.UI.Screen.Splash
import           Labyrinth.UI.Screen.Registration

data Screen e = Splash SplashScreen | Registration (RegistrationScreen e)


attributeMap :: s -> AttrMap
attributeMap = const $ attrMap
  V.defAttr
  [ (E.editAttr          , V.white `on` V.black)
  , (E.editFocusedAttr   , V.black `on` V.white)
  , (focusedFormInputAttr, V.black `on` V.white)
  , ("Yellow"            , V.black `on` V.yellow)
  , ("Blue"              , V.black `on` V.blue)
  , ("Green"             , V.black `on` V.green)
  , ("Red"               , V.black `on` V.red)
  ]
