module Labyrinth.Store.RegistrationEvent
  ( handle
  )
where

import           Brick
import           Brick.Forms                    ( handleFormEvent )
import qualified Graphics.Vty                  as V
import           Lens.Micro                     ( (^.)
                                                , (&)
                                                , (.~)
                                                )
import           Labyrinth.Store.Internal       ( EventHandler
                                                , State(..)
                                                , state
                                                )
import           Labyrinth.Screens              ( RegistrationScreen )
import           Labyrinth.Screens.Registration ( form )

handle :: EventHandler (RegistrationScreen e) e
handle store screen ev = case ev of
  VtyEvent (V.EvKey V.KEsc []) -> halt store
  _                            -> do
    form' <- handleFormEvent ev (screen ^. form)
    let screen' = screen & form .~ form'
        store'  = store & state .~ Registration screen'
    continue store'
