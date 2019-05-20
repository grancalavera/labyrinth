module Labyrinth.Store.Event.Modal
  ( handle
  , isModalEvent
  , showModal
  , nextModal
  )
where

import           Brick
import qualified Brick.Widgets.Dialog          as D
import           Control.Lens                     ( (.~)
                                                , (&)
                                                , (^.)
                                                , (%~)
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
handle store ev = (fromMaybe handleInModal $ eventMap !? ev) store ev

handleInModal :: GlobalEventHandler e
handleInModal store ev = maybe (continue store) withModal (nextModal store)
 where
  withModal m = case ev of
    (VtyEvent (V.EvKey V.KEsc   [])) -> (hideModalAnd $ m ^. onFalse) store
    (VtyEvent (V.EvKey V.KEnter [])) -> fromMaybe (continue store) $ do
      sel <- D.dialogSelection (m ^. dialog)
      return $ if sel
        then (hideModalAnd $ m ^. onTrue) store
        else (hideModalAnd $ m ^. onFalse) store
    (VtyEvent vtyEv) -> do
      d <- D.handleDialogEvent vtyEv (m ^. dialog)
      continue $ store & modals %~ \case
        []       -> []
        (_ : ms) -> (m & dialog .~ d) : ms
    _ -> continue store

promptToQuit :: GlobalEventHandler e
promptToQuit store _ = showModal store $ mkModal
  "quit"
  (txt "Do you want to quit Labyrinth?")
  (0, [("Stay", False), ("Quit", True)])
  halt
  continue

eventMap :: Ord e => Map (BrickEvent Name e) (GlobalEventHandler e)
eventMap =
  Map.fromList [(VtyEvent (V.EvKey (V.KChar 'q') [V.MCtrl]), promptToQuit)]
