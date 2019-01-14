module Labyrinth.Store.Event.Setup
  ( handle
  )
where

import           Data.Maybe                     ( maybe )
import           Brick
import           Brick.Forms                    ( handleFormEvent )
import qualified Graphics.Vty                  as V
import           Lens.Micro                     ( (&)
                                                , (.~)
                                                )
import           Labyrinth.Store.Internal
import           Labyrinth.UI                   ( SetupS
                                                , ModalCallback
                                                )
import qualified Labyrinth.UI.Screen.Setup     as S
import qualified Labyrinth.UI.Screen.Game      as G
import qualified Labyrinth.UI.Widget           as UI
import qualified Labyrinth.UI.Modal            as UI
import           Labyrinth.Game                 ( PlayOrder(..)
                                                , Game(..)
                                                , Player
                                                , Players
                                                )

type RegistrationEventHandler e = EventHandler (SetupS e) e

handle :: RegistrationEventHandler e
handle s store ev = handleEvent s store ev
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
submit s store _ =
  continue $ if S.validate s then update store (S.submitPlayer s) else store

play :: RegistrationEventHandler e
play s store _ = maybe (continue store) promptToPlay (S.setup s)
 where
  promptToPlay (p, ps) = showModal store
    $ UI.mkOkModal "start" (UI.nextPlayerPrompt p) (continueToNewGame p ps)

processInput :: RegistrationEventHandler e
processInput s store ev =
  S.processForm s (handleFormEvent ev) >>= continue . update store

edit :: PlayOrder -> RegistrationEventHandler e
edit i s store _ =
  continue $ update store $ maybe s (S.editPlayer s) (S.playerAt s i)

update :: Store e -> SetupS e -> Store e
update store s = store & state .~ Setup s

continueToNewGame :: Player -> Players -> ModalCallback Store e
continueToNewGame _ _ store =
  halt store
