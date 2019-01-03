module Main where

import           Brick
import           Graphics.Vty                   ( Vty )
import qualified Graphics.Vty                  as V
import           Lens.Micro                     ( (^.) )
import           Data.Maybe                     ( fromMaybe )
import           Control.Monad                  ( void )
import qualified Labyrinth.UI                  as UI
import           Labyrinth.UI                   ( Name
                                                , Screen(..)
                                                )
import           Labyrinth.UI.Widget
import qualified Labyrinth.UI.Modal            as Modal
import qualified Labyrinth.Store.Event.Modal   as Modal
import qualified Labyrinth.UI.Screen.Splash    as Splash
import qualified Labyrinth.Store.Event.Splash  as Splash

import qualified Labyrinth.UI.Screen.Registration
                                               as Registration
import qualified Labyrinth.Store.Event.Registration
                                               as Registration

import qualified Labyrinth.Store               as Store
import           Labyrinth.Store                ( Store
                                                , Ev
                                                , state
                                                , modal
                                                )

main :: IO ()
main = void $ customMain buildVty Nothing app Store.initial

app :: App (Store Ev) Ev Name
app = App { appDraw         = draw
          , appChooseCursor = chooseCursor
          , appHandleEvent  = handleEvent
          , appStartEvent   = return
          , appAttrMap      = UI.attributeMap
          }

draw :: Store e -> [Widget Name]
draw store = [maybe screen Modal.draw (store ^. modal)]
 where
  screen = appContainer 50 $ case store ^. state of
    Splash       s -> Splash.draw s
    Registration s -> Registration.draw s

handleEvent
  :: Ord e => Store e -> BrickEvent Name e -> EventM Name (Next (Store e))
handleEvent store ev = handle store ev
 where
  handle = if Store.isModalEvent store ev
    then Modal.handle
    else case store ^. state of
      Splash       screen -> Splash.handle screen
      Registration screen -> Registration.handle screen

buildVty :: IO Vty
buildVty = do
  v <- V.mkVty =<< V.standardIOConfig
  V.setMode (V.outputIface v) V.Mouse True
  return v

chooseCursor :: Store e -> [CursorLocation Name] -> Maybe (CursorLocation Name)
chooseCursor store =
  fromMaybe (neverShowCursor store) $ if Store.isShowingModal store
    then store ^. modal >>= Modal.chooseCursor
    else case store ^. state of
      Splash       screen -> Splash.chooseCursor screen
      Registration screen -> Registration.chooseCursor screen
