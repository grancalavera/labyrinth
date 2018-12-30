module Labyrinth.Store
  ( Store
  , state
  , initial
  )
where

import           Labyrinth.Store.Internal
import           Labyrinth.UI                   ( Screen(..) )
import qualified Labyrinth.UI.Screen.Splash    as Splash

initial :: Store e
initial = Store $ Splash Splash.initialScreen
