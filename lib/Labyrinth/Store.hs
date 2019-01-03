module Labyrinth.Store
  ( Store
  , Ev
  , state
  , modal
  , initial
  , isModalEvent
  , isShowingModal
  )
where

import           Brick
import           Lens.Micro                     ( (^.) )
import           Data.Maybe                     ( isJust )
import           Labyrinth.Store.Internal
import qualified Labyrinth.Store.Event.Modal   as Modal
import           Labyrinth.UI                   ( Name
                                                , Screen(..)
                                                )
import qualified Labyrinth.UI.Screen.Splash    as Splash

initial :: Store e
initial = Store { _state = Splash Splash.initial, _modal = Nothing }

isModalEvent :: Ord e => Store e -> BrickEvent Name e -> Bool
isModalEvent store ev = isShowingModal store || Modal.isModalEvent ev

isShowingModal :: Store e -> Bool
isShowingModal = isJust . (^. modal)
