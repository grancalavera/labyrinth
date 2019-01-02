module Labyrinth.Store
  ( Store
  , Ev
  , state
  , global
  , initial
  , handleGlobalEvent
  )
where

import           Brick
import           Lens.Micro                     ( (^.) )
import           Labyrinth.Store.Internal
import qualified Labyrinth.Store.Event.Global  as Global
import qualified Labyrinth.UI.Global           as Global
import           Labyrinth.UI                   ( Name
                                                , Screen(..)
                                                )
import qualified Labyrinth.UI.Screen.Splash    as Splash

initial :: Store e
initial = Store { _state = Splash Splash.initial, _global = Global.initial }

handleGlobalEvent :: Ord e => Store e -> BrickEvent Name e -> Bool
handleGlobalEvent store ev =
  Global.screenIsBlocked (store ^. global) || Global.isGlobalEvent ev
