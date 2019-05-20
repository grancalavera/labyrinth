module Labyrinth.Store.Event.Setup
  ( handle
  )
where

import           Brick
import           Brick.Forms                              ( handleFormEvent )
import           Data.Maybe                               ( maybe )
import qualified Graphics.Vty                  as V
import           Labyrinth.Game                           ( PlayOrder(..) )
import           Labyrinth.Store.Internal
import           Labyrinth.UI                             ( SetupS )
import qualified Labyrinth.UI.Screen.Setup     as S
import           Control.Lens                               ( (&)
                                                          , (.~)
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

play :: RegistrationEventHandler e
play s store _ = maybe (continue store) beginGame (S.players s)
  where beginGame _ = halt store

edit :: PlayOrder -> RegistrationEventHandler e
edit i s store _ =
  continue $ update store $ maybe s (S.editPlayer s) (S.playerAt s i)

submit :: RegistrationEventHandler e
submit s store _ =
  continue $ if S.validate s then update store (S.submitPlayer s) else store

processInput :: RegistrationEventHandler e
processInput s store ev =
  S.processForm s (handleFormEvent ev) >>= continue . update store

update :: Store e -> SetupS e -> Store e
update store s = store & state .~ Setup s
