module Labyrinth.Store.Event.Setup
  ( handle
  )
where

import           Brick
import           Brick.Forms                    ( handleFormEvent )
import qualified Graphics.Vty                  as V
import           Lens.Micro                     ( (&)
                                                , (.~)
                                                , (^.)
                                                )
import           Labyrinth.Store.Internal
import           Labyrinth.UI                   ( SetupS )
import           Labyrinth.UI.Screen.Setup      ( submitPlayer
                                                , validate
                                                , editPlayer
                                                , playerAt
                                                , processForm
                                                , hasEnoughPlayers
                                                , firstPlayer
                                                )
import           Labyrinth.UI.Widget            ( playerAttr )
import           Labyrinth.UI.Modal             ( mkOkModal )
import qualified Labyrinth.Game.Configuration  as Conf
import           Labyrinth.Game.Configuration   ( PlayOrder(..) )

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
  continue $ if validate s then update store (submitPlayer s) else store

play :: RegistrationEventHandler e
play s store _ = if hasEnoughPlayers s then start else continue store
 where
  start = maybe (continue store) promptToStart (firstPlayer s)
  promptToStart p = showModal store $ mkOkModal
    "start"
    (txt "The next player is " <+> playerAttr
      p
      (padLeft (Pad 1) $ padRight (Pad 1) $ txt $ p ^. Conf.name)
    )
    halt

processInput :: RegistrationEventHandler e
processInput s store ev =
  processForm s (handleFormEvent ev) >>= continue . update store

edit :: PlayOrder -> RegistrationEventHandler e
edit i s store _ =
  continue $ update store $ maybe s (editPlayer s) (playerAt s i)

update :: Store e -> SetupS e -> Store e
update store s = store & state .~ Setup s
