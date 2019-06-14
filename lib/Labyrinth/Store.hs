module Labyrinth.Store
  ( Store
  , State(..)
  , Ev
  , state
  , modals
  , initial
  , isModalEvent
  , isShowingModal
  , nextModal
  )
where

import           Brick
import           Control.Lens                                                 ( (^.) )
import           Labyrinth.Store.Internal
import           Labyrinth.UI                                                 ( Name )

initial :: Store e
initial = Store { _state = Splash, _modals = [] }

isModalEvent :: Store e -> BrickEvent Name e -> Bool
isModalEvent store _ = isShowingModal store

isShowingModal :: Store e -> Bool
isShowingModal = not . null . (^. modals)
