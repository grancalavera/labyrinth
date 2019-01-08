module Labyrinth.Store.Event.Modal
  ( handle
  , isModalEvent
  )
where

import           Brick
import qualified Brick.Widgets.Dialog          as D
import           Lens.Micro                     ( (?~)
                                                , (.~)
                                                , (&)
                                                , (^.)
                                                )
import qualified Graphics.Vty                  as V
import qualified Data.Map.Strict               as Map
import           Data.Map.Strict                ( Map
                                                , (!?)
                                                )
import           Data.Maybe                     ( fromMaybe )
import           Labyrinth.UI                   ( Name )
import           Labyrinth.UI.Modal             ( Modal
                                                , ModalCallback
                                                , dialog
                                                , onTrue
                                                , onFalse
                                                , showModal
                                                )
import           Labyrinth.Store.Internal

type GlobalEventHandler e
  = Store e -> BrickEvent Name e -> EventM Name (Next (Store e))

isModalEvent :: Ord e => BrickEvent Name e -> Bool
isModalEvent = (`elem` Map.keys eventMap)

handle :: Ord e => GlobalEventHandler e
handle store ev = (fromMaybe handleWithModal $ eventMap !? ev) store ev

handleWithModal :: GlobalEventHandler e
handleWithModal store ev = maybe (continue store) withModal (store ^. modal)
 where
  withModal modal' = case ev of
    (VtyEvent (V.EvKey V.KEsc   [])) -> modal' ^. onFalse
    (VtyEvent (V.EvKey V.KEnter [])) -> fromMaybe (continue store) $ do
      sel <- D.dialogSelection (modal' ^. dialog)
      return $ if sel then modal' ^. onTrue else modal' ^. onFalse
    (VtyEvent vtyEv) -> do
      dialog' <- D.handleDialogEvent vtyEv (modal' ^. dialog)
      continue $ store & modal ?~ (modal' & dialog .~ dialog')
    _ -> continue store

promptToQuit :: GlobalEventHandler e
promptToQuit store _ = continue $ store & modal ?~ quitPrompt onT onF
 where
  onT = halt store
  onF = continue $ store & modal .~ Nothing

eventMap :: Ord e => Map (BrickEvent Name e) (GlobalEventHandler e)
eventMap =
  Map.fromList [(VtyEvent (V.EvKey (V.KChar 'q') [V.MCtrl]), promptToQuit)]

quitPrompt :: ModalCallback Store e -> ModalCallback Store e -> Modal Store e
quitPrompt = showModal body options
 where
  body    = txt "Do you want to quit Labyrinth?"
  options = (0, [("Stay", False), ("Quit", True)])
