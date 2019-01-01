module Main where

import           Brick
import           Graphics.Vty                   ( Vty )
import qualified Graphics.Vty                  as V
import           Lens.Micro                     ( (^.) )
-- import           Control.Monad                  ( void )
import           Data.Maybe                     ( fromMaybe )



import qualified Labyrinth.UI                  as UI
import           Labyrinth.UI                   ( ResourceName
                                                , Screen(..)
                                                )

import qualified Labyrinth.UI.Screen.Splash    as Splash
import qualified Labyrinth.Store.Event.Splash  as Splash

import qualified Labyrinth.UI.Screen.Registration
                                               as Registration
import qualified Labyrinth.Store.Event.Registration
                                               as Registration

import qualified Labyrinth.Store               as Store
import           Labyrinth.Store                ( Store
                                                , state
                                                )

main :: IO ()
main = do
  store <- customMain buildVty Nothing app Store.initial
  case store ^. state of

    Splash _ -> putStrLn $ "\n\n\n" <> "Finished at Splash." <> "\n\n\n"

    Registration screen ->
      putStrLn
        $  "\n\n\n"
        <> "Finished at Registration:\n"
        <> (show $ screen ^. Registration.players)
        <> "\n\n\n"


app :: App (Store e) e ResourceName
app = App { appDraw         = draw
          , appChooseCursor = chooseCursor
          , appHandleEvent  = handleEvent
          , appStartEvent   = return
          , appAttrMap      = UI.attributeMap
          }

draw :: Store e -> [Widget ResourceName]
draw store = case store ^. state of
  Splash       screen -> Splash.draw screen
  Registration screen -> Registration.draw screen

handleEvent
  :: Store e
  -> BrickEvent ResourceName e
  -> EventM ResourceName (Next (Store e))
handleEvent store = handle store
 where
  handle = case store ^. state of
    Splash       screen -> Splash.handle screen
    Registration screen -> Registration.handle screen

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
  Registration screen -> fromMaybe noCursor $ Registration.chooseCursor screen
  _                   -> noCursor
  where noCursor = neverShowCursor store
