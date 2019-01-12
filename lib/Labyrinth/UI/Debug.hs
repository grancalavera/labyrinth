module Labyrinth.UI.Debug
  ( draw
  )
where

import           Brick
import           Lens.Micro                     ( (^.) )
import qualified Labyrinth.Store               as S
import           Labyrinth.Store                ( Store, State(..) )

draw :: Store e -> Widget n
draw store = vBox [state, modals]
  where
    state = txt $ case (store ^. S.state) of
      Splash _ -> "State: Splash"
      Setup _ -> "State: Setup"
      Plan _ -> "State: Plan"
      _ -> "State: not implemented"
    modals = str $ show $ store ^. S.modal
