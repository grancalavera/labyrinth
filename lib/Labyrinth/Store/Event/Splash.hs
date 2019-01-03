module Labyrinth.Store.Event.Splash
  ( handle
  )
where

import           Brick
import qualified Graphics.Vty                  as V
import           Lens.Micro                     ( (&)
                                                , (.~)
                                                )
import           Labyrinth.Store.Internal       ( EventHandler
                                                , state
                                                )
import           Labyrinth.UI                   ( Screen(..)
                                                , SplashScreen
                                                )
import qualified Labyrinth.UI.Screen.Registration
                                               as Registration

handle :: EventHandler SplashScreen e
handle _ store ev = case ev of
  VtyEvent (V.EvKey V.KEnter []) -> continue toRegistration
  _                              -> continue store
 where
  toRegistration = store & state .~ Registration Registration.initial
