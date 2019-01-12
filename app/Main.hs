module Main where

import           Brick
import           Graphics.Vty                   ( Vty )
import qualified Graphics.Vty                  as V
import           Lens.Micro                     ( (^.) )
import           Data.Maybe                     ( fromMaybe )

import qualified Labyrinth.UI                  as UI
import qualified Labyrinth.UI.Widget           as UI
import           Labyrinth.UI                   ( Name )

import qualified Labyrinth.UI.Debug            as Debug

import qualified Labyrinth.UI.Modal            as Modal
import qualified Labyrinth.Store.Event.Modal   as Modal

import qualified Labyrinth.UI.Screen.Splash    as Splash
import qualified Labyrinth.Store.Event.Splash  as Splash

import qualified Labyrinth.UI.Screen.Setup     as Setup
import qualified Labyrinth.Store.Event.Setup   as Setup

import qualified Labyrinth.Store               as Store
import           Labyrinth.Store                ( Store
                                                , State(..)
                                                , Ev
                                                )

main :: IO ()
main = do
  store <- customMain buildVty Nothing app Store.initial
  print store

app :: App (Store Ev) Ev Name
app = App { appDraw         = draw
          , appChooseCursor = chooseCursor
          , appHandleEvent  = handleEvent
          , appStartEvent   = return
          , appAttrMap      = UI.attributeMap
          }

draw :: Store e -> [Widget Name]
draw store =
  [Debug.draw store, maybe drawScreen Modal.draw (Store.nextModal store)]
 where
  drawScreen = UI.appContainer 50 $ case store ^. Store.state of
    Splash s -> Splash.draw s
    Setup  s -> Setup.draw s
    _        -> txt "Screen not implemented"

handleEvent
  :: Ord e => Store e -> BrickEvent Name e -> EventM Name (Next (Store e))
handleEvent store ev = handle store ev
 where
  handle = if Store.isModalEvent store ev
    then Modal.handle
    else case store ^. Store.state of
      Splash s -> Splash.handle s
      Setup  s -> Setup.handle s
      _        -> \_ _ -> halt store

buildVty :: IO Vty
buildVty = do
  v <- V.mkVty =<< V.standardIOConfig
  V.setMode (V.outputIface v) V.Mouse True
  return v

chooseCursor :: Store e -> [CursorLocation Name] -> Maybe (CursorLocation Name)
chooseCursor store =
  fromMaybe (neverShowCursor store) $ if Store.isShowingModal store
    then Store.nextModal store >>= Modal.chooseCursor
    else case store ^. Store.state of
      Setup s -> Setup.chooseCursor s
      _       -> Nothing
