module Labyrinth.UI.Debug
  ( draw
  )
where

import           Brick
import           Lens.Micro                     ( (^.) )
import qualified Labyrinth.Store               as S
import           Labyrinth.Store                ( Store
                                                , State(..)
                                                )

draw :: Store e -> Widget n
draw store = vBox [state, modals]
 where
  state = txt . ("State: " <>) $ case store ^. S.state of
    Splash _ -> "Splash"
    Setup  _ -> "Setup"
    Plan   _ -> "Plan"
    _        -> "not implemented"
  modals = str $ show $ store ^. S.modals