module Labyrinth.Store.Event.Splash
  ( handle
  )
where

import           Brick
import qualified Graphics.Vty                  as V
import           Lens.Micro                     ( (&)
                                                , (.~)
                                                )
import           Labyrinth.Store.Internal


import           Labyrinth.UI                   ( SplashS )
import qualified Labyrinth.UI.Screen.Setup     as Setup

handle :: EventHandler SplashS e
handle _ store ev = case ev of
  VtyEvent (V.EvKey V.KEnter []) -> continue toRegistration
  _                              -> continue store
  where toRegistration = store & state .~ Setup Setup.initial
