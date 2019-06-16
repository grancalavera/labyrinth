module Labyrinth.UI
  ( SplashS
  , SetupS
  , GameS
  , Modal(..)
  , Name(..)
  , ModalCallback
  , attributeMap
  )
where

import           Brick
import qualified Brick.Widgets.Dialog          as D
import qualified Brick.Widgets.Edit            as E
import           Brick.Forms                                        ( focusedFormInputAttr
                                                                    )
import qualified Graphics.Vty                  as V

import           Labyrinth.UI.Internal
import           Labyrinth.UI.Modal
import           Labyrinth.UI.Screen.Splash
import           Labyrinth.UI.Screen.Setup
import           Labyrinth.UI.Screen.Game

attributeMap :: s -> AttrMap
attributeMap = const $ attrMap
  V.defAttr
  [ (E.editAttr          , V.white `on` V.black)
  , (E.editFocusedAttr   , V.black `on` V.white)
  , (focusedFormInputAttr, V.black `on` V.white)
  , (D.buttonSelectedAttr, V.black `on` V.white)
  , ("Yellow"            , V.black `on` V.yellow)
  , ("Blue"              , V.black `on` V.blue)
  , ("Green"             , V.black `on` V.green)
  , ("Red"               , V.black `on` V.red)
  ]
