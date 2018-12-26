module Main where

import           Brick
import           Graphics.Vty                   ( Vty )
import qualified Graphics.Vty                  as V
import           Lens.Micro                     ( (^.) )
import           Control.Monad                  ( void )

import qualified Labyrinth.Screens             as Screens
import           Labyrinth.Screens              ( ResourceName
                                                , SplashScreen(..)
                                                )
import qualified Labyrinth.Screens.Registration
                                               as Registration
import qualified Labyrinth.Screens.Splash      as Splash
import qualified Labyrinth.Store.SplashEvent   as SplashEvent
import qualified Labyrinth.Store.RegistrationEvent
                                               as RegistrationEvent
import           Labyrinth.Store                ( Store(..)
                                                , State(..)
                                                , state
                                                )

main :: IO ()
main = do
  let store = Store (Splash SplashScreen)
  void $ customMain buildVty Nothing app store

app :: App (Store e) e ResourceName
app = App { appDraw         = draw
          , appChooseCursor = chooseCursor
          , appHandleEvent  = handleEvent
          , appStartEvent   = return
          , appAttrMap      = Screens.attributeMap
          }

draw :: Store e -> [Widget ResourceName]
draw store = case store ^. state of
  Splash       screen -> Splash.draw screen
  Registration screen -> Registration.draw screen

handleEvent
  :: Store e
  -> BrickEvent ResourceName e
  -> EventM ResourceName (Next (Store e))
handleEvent store ev = case store ^. state of
  Splash       screen -> SplashEvent.handle store screen ev
  Registration screen -> RegistrationEvent.handle store screen ev

buildVty :: IO Vty
buildVty = do
  v <- V.mkVty =<< V.standardIOConfig
  V.setMode (V.outputIface v) V.Mouse True
  return v

chooseCursor
  :: Store e
  -> [CursorLocation ResourceName]
  -> Maybe (CursorLocation ResourceName)
chooseCursor store = case store ^. state of
  Registration screen -> Registration.chooseCursor screen
  _                   -> neverShowCursor store
