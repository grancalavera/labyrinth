module Labyrinth.Store.Event.Registration
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
                                                , state
                                                )
import           Labyrinth.UI                   ( Screen(..)
                                                , RegistrationScreen
                                                )
import           Labyrinth.UI.Screen.Registration
                                                ( form
                                                , submit
                                                , hasValidName
                                                , hasEnoughPlayers
                                                )

handle :: EventHandler (RegistrationScreen e) e
handle screen store ev = case screen ^. form of

  Just form' -> case ev of

    VtyEvent (V.EvKey V.KEsc []) -> halt store

    VtyEvent (V.EvKey (V.KChar 'a') [V.MCtrl]) ->
      continue
        $ if hasValidName screen then updateStore (submit screen) else store

    VtyEvent (V.EvKey (V.KChar 'p') [V.MCtrl]) ->
      if hasEnoughPlayers screen then halt store else continue store

    _ -> do
      form'' <- handleFormEvent ev form'
      continue $ updateStore (screen & form ?~ form'')

  Nothing -> case ev of
    VtyEvent (V.EvKey V.KEsc []) -> halt store
    _                            -> continue store
  where updateStore s = store & state .~ Registration s
