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
                                                , (?~)
                                                )
import           Labyrinth.Store.Internal       ( EventHandler
                                                , State(..)
                                                , state
                                                )
import           Labyrinth.Screens              ( RegistrationScreen )
import           Labyrinth.Screens.Registration ( form
                                                , submit
                                                )

handle :: EventHandler (RegistrationScreen e) e
handle store screen ev = case screen ^. form of

  Just form' -> case ev of

    VtyEvent (V.EvKey V.KEsc   []) -> halt store
    VtyEvent (V.EvKey V.KEnter []) -> continue $ newStore (submit screen)
    _                              -> do
      form'' <- handleFormEvent ev form'
      continue $ newStore (screen & form ?~ form'')

  Nothing -> case ev of
    VtyEvent (V.EvKey V.KEsc []) -> halt store
    _                            -> continue store
  where newStore newScreen = store & state .~ Registration newScreen
