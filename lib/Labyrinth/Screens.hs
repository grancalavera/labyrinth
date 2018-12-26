module Labyrinth.Screens
  ( ResourceName
  , RegistrationFormOptions
  , RegistrationScreen(..)
  , SplashScreen(..)
  , attributeMap
  )
where

import           Brick
import qualified Brick.Widgets.Edit            as E
import           Brick.Forms                    ( focusedFormInputAttr )
import qualified Graphics.Vty                  as V
import           Labyrinth.Screens.Internal     ( ResourceName )
import           Labyrinth.Screens.Registration ( RegistrationFormOptions
                                                , RegistrationScreen(..)
                                                )
import           Labyrinth.Screens.Splash       ( SplashScreen(..) )

attributeMap :: s -> AttrMap
attributeMap = const $ attrMap
  V.defAttr
  [ (E.editAttr          , V.white `on` V.black)
  , (E.editFocusedAttr   , V.black `on` V.yellow)
  , (focusedFormInputAttr, V.black `on` V.yellow)
  ]
