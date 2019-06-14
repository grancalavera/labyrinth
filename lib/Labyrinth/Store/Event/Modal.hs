module Labyrinth.Store.Event.Modal
  ( handle
  , showModal
  , nextModal
  , promptToQuit
  )
where

import           Brick
import qualified Brick.Widgets.Dialog          as D
import           Control.Lens                                       ( (.~)
                                                                    , (&)
                                                                    , (^.)
                                                                    , (%~)
                                                                    )
import qualified Graphics.Vty                  as V
import           Data.Maybe                                         ( fromMaybe )
import           Labyrinth.UI                                       ( Name )
import           Labyrinth.UI.Modal                                 ( dialog
                                                                    , onTrue
                                                                    , onFalse
                                                                    , mkModal
                                                                    )
import           Labyrinth.Store.Internal

handle :: Store e -> BrickEvent Name e -> EventM Name (Next (Store e))
handle store ev = maybe (continue store) withModal (nextModal store)
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

promptToQuit :: Store e -> EventM Name (Next (Store e))
promptToQuit store = showModal store $ mkModal "quit"
                                               (txt "Do you want to quit Labyrinth?")
                                               (0, [("Stay", False), ("Quit", True)])
                                               halt
                                               continue
