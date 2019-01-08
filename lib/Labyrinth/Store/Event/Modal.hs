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
import           Labyrinth.UI.Modal             ( dialog
                                                , onTrue
                                                , onFalse
                                                , mkModal
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
promptToQuit store _ = showModal store $ mkModal
  (txt "Do you want to quit Labyrinth?")
  (0, [("Stay", False), ("Quit", True)])
  (halt store)
  (hideModal store)

eventMap :: Ord e => Map (BrickEvent Name e) (GlobalEventHandler e)
eventMap =
  Map.fromList [(VtyEvent (V.EvKey (V.KChar 'q') [V.MCtrl]), promptToQuit)]
