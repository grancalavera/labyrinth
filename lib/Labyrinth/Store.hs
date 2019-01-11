module Labyrinth.Store
  ( Store
  , State(..)
  , Ev
  , state
  , modal
  , initial
  , isModalEvent
  , isShowingModal
  , nextModal
  )
where

import           Brick
import           Lens.Micro                     ( (^.) )
import           Labyrinth.Store.Internal
import qualified Labyrinth.Store.Event.Modal   as Modal
import           Labyrinth.UI                   ( Name )
import qualified Labyrinth.UI.Screen.Splash    as Splash

initial :: Store e
initial = Store { _state = Splash Splash.initial, _modal = [] }

isModalEvent :: Ord e => Store e -> BrickEvent Name e -> Bool
isModalEvent store ev = isShowingModal store || Modal.isModalEvent ev

isShowingModal :: Store e -> Bool
isShowingModal = not . null . (^. modal)
