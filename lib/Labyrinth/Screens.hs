module Labyrinth.Screens
  ( ResourceName
  , RegistrationScreen
  , SplashScreen
  , attributeMap
  )
where

import           Brick
import qualified Brick.Widgets.Edit            as E
import           Brick.Forms                    ( focusedFormInputAttr )
import qualified Graphics.Vty                  as V

import           Labyrinth.Screens.Internal
import           Labyrinth.Screens.Splash
import           Labyrinth.Screens.Registration

attributeMap :: s -> AttrMap
attributeMap = const $ attrMap
  V.defAttr
  [ (E.editAttr          , V.white `on` V.black)
  , (E.editFocusedAttr   , V.black `on` V.white)
  , (focusedFormInputAttr, V.black `on` V.white)
  ]
