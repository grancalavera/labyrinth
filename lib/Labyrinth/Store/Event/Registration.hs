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
import           Labyrinth.Store.Internal
import           Labyrinth.UI                   ( Screen(Registration)
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
import           Labyrinth.Game.Configuration   ( PlayOrder(..) )

type RegistrationEventHandler e = EventHandler (RegistrationScreen e) e

handle :: RegistrationEventHandler e
handle screen store ev = handleEvent screen store ev
 where
  handleEvent = case ev of
    VtyEvent (V.EvKey (V.KChar 'p') [V.MCtrl]) -> play
    VtyEvent (V.EvKey (V.KChar 'a') [V.MCtrl]) -> edit First
    VtyEvent (V.EvKey (V.KChar 's') [V.MCtrl]) -> edit Second
    VtyEvent (V.EvKey (V.KChar 'd') [V.MCtrl]) -> edit Third
    VtyEvent (V.EvKey (V.KChar 'f') [V.MCtrl]) -> edit Fourth
    VtyEvent (V.EvKey V.KEnter []) -> submit
    _ -> processInput

submit :: RegistrationEventHandler e
submit screen store _ = continue
  $ if validate screen then update store (submitPlayer screen) else store

play :: RegistrationEventHandler e
play screen store _ =
  if hasEnoughPlayers screen then halt store else continue store

processInput :: RegistrationEventHandler e
processInput screen store ev =
  processForm screen (handleFormEvent ev) >>= continue . update store

edit :: PlayOrder -> RegistrationEventHandler e
edit i screen store _ =
  continue $ update store $ maybe screen (editPlayer screen) (playerAt screen i)

update :: Store e -> RegistrationScreen e -> Store e
update store screen = store & state .~ Registration screen
