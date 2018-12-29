module Labyrinth.Store.SplashEvent
  ( handle
  )
where

import           Brick
import qualified Graphics.Vty                  as V
import           Lens.Micro                     ( (&)
                                                , (.~)
                                                )
import           Labyrinth.Store.Internal       ( EventHandler
                                                , State(..)
                                                , state
                                                )
import           Labyrinth.Screens              ( SplashScreen )
import qualified Labyrinth.Screens.Registration
                                               as Registration

handle :: EventHandler SplashScreen e
handle _ store ev = case ev of
  VtyEvent (V.EvKey V.KEsc   []) -> halt store
  VtyEvent (V.EvKey V.KEnter []) -> continue toRegistration
  _                              -> continue store
 where
  toRegistration = store & state .~ Registration Registration.initialScreen
