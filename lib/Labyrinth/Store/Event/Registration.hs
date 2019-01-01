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
                                                ( submit
                                                , validate
                                                , processForm
                                                , hasEnoughPlayers
                                                )

type RegistrationEventHandler e = EventHandler (RegistrationScreen e) e

handle :: RegistrationEventHandler e
handle screen store ev = handleEvent screen store ev
 where
  handleEvent = case ev of
    VtyEvent (V.EvKey (V.KChar 'q') [V.MCtrl]) -> quit
    VtyEvent (V.EvKey (V.KChar 'p') [V.MCtrl]) -> beginGame
    VtyEvent (V.EvKey V.KEnter []) -> submitPlayer
    _ -> processInput

quit :: RegistrationEventHandler e
quit _ store _ = halt store

submitPlayer :: RegistrationEventHandler e
submitPlayer screen store _ =
  continue $ if validate screen then update store (submit screen) else store

beginGame :: RegistrationEventHandler e
beginGame screen store _ =
  if hasEnoughPlayers screen then halt store else continue store

processInput :: RegistrationEventHandler e
processInput screen store ev =
  processForm screen (handleFormEvent ev) >>= continue . update store

update :: Store e -> RegistrationScreen e -> Store e
update store screen = store & state .~ Registration screen
