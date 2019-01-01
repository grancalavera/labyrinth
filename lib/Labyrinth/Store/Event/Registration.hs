module Labyrinth.Store.Event.Registration
  ( handle
  )
where

import           Brick
import           Brick.Forms                    ( handleFormEvent )
import qualified Graphics.Vty                  as V
import           Lens.Micro                     ( (&)
                                                , (.~)
                                                )
import           Labyrinth.Store.Internal       ( EventHandler
                                                , Store
                                                , state
                                                )
import           Labyrinth.UI                   ( Screen(..)
                                                , RegistrationScreen
                                                )
import           Labyrinth.UI.Screen.Registration
                                                ( submitPlayer
                                                , validate
                                                , editPlayer
                                                , playerAt
                                                , processForm
                                                , hasEnoughPlayers
                                                )

type RegistrationEventHandler e = EventHandler (RegistrationScreen e) e

handle :: RegistrationEventHandler e
handle screen store ev = handleEvent screen store ev
 where
  handleEvent = case ev of
    VtyEvent (V.EvKey (V.KChar 'q') [V.MCtrl]) -> quit
    VtyEvent (V.EvKey (V.KChar 'p') [V.MCtrl]) -> play
    VtyEvent (V.EvKey (V.KChar 'a') [V.MCtrl]) -> edit 0
    VtyEvent (V.EvKey (V.KChar 's') [V.MCtrl]) -> edit 1
    VtyEvent (V.EvKey (V.KChar 'd') [V.MCtrl]) -> edit 2
    VtyEvent (V.EvKey (V.KChar 'f') [V.MCtrl]) -> edit 3
    VtyEvent (V.EvKey V.KEnter []) -> submit
    _ -> processInput

quit :: RegistrationEventHandler e
quit _ store _ = halt store

submit :: RegistrationEventHandler e
submit screen store _ = continue
  $ if validate screen then update store (submitPlayer screen) else store

play :: RegistrationEventHandler e
play screen store _ =
  if hasEnoughPlayers screen then halt store else continue store

processInput :: RegistrationEventHandler e
processInput screen store ev =
  processForm screen (handleFormEvent ev) >>= continue . update store

edit :: Int -> RegistrationEventHandler e
edit i screen store _ =
  continue $ update store $ maybe screen (editPlayer screen) (playerAt screen i)

update :: Store e -> RegistrationScreen e -> Store e
update store screen = store & state .~ Registration screen
