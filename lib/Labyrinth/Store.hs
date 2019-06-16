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
import           Control.Lens                                       ( (^.) )
import           Labyrinth.Store.Internal
import           Labyrinth.UI                                       ( Name )

import           Labyrinth.UI.Modal                                 ( Modal )
import qualified Labyrinth.UI.Modal            as Modal

initial :: Store e
initial = Store { _state = Splash, _modals = [welcome] }

isModalEvent :: Store e -> BrickEvent Name e -> Bool
isModalEvent store _ = isShowingModal store

isShowingModal :: Store e -> Bool
isShowingModal = not . null . (^. modals)

welcome :: Modal Store e
welcome = Modal.mkOkModal "welcome" (txt "Welcome!") (hideModalAnd continue)
