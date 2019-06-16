module Labyrinth.Store
  ( Store
  , State(..)
  , Ev
  , state
  , modals
  , initial
  , isShowingModal
  , nextModal
  )
where

import           Brick
import           Control.Lens                                       ( (^.) )
import           Labyrinth.Store.Internal
import           Labyrinth.UI.Modal                                 ( Modal )
import qualified Labyrinth.UI.Modal            as Modal

initial :: Store e
initial = Store { _state = Splash, _modals = [welcome] }

isShowingModal :: Store e -> Bool
isShowingModal = not . null . (^. modals)

welcome :: Modal Store e
welcome = Modal.mkOkModal "welcome" (txt "Welcome!") (hideModalAnd continue)
