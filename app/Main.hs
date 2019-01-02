module Main where

import           Brick
import           Graphics.Vty                   ( Vty )
import qualified Graphics.Vty                  as V
import           Lens.Micro                     ( (^.) )
import           Data.Maybe                     ( fromMaybe )

import qualified Labyrinth.UI                  as UI
import           Labyrinth.UI                   ( Name
                                                , Screen(..)
                                                )
import           Labyrinth.UI.Widget
import qualified Labyrinth.Store.Event.Global  as Global
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


app :: App (Store Ev) Ev Name
app = App { appDraw         = draw
          , appChooseCursor = chooseCursor
          , appHandleEvent  = handleEvent
          , appStartEvent   = return
          , appAttrMap      = UI.attributeMap
          }

draw :: Store e -> [Widget Name]
draw store = [appContainer 50 screen]
 where
  screen = case store ^. state of
    Splash       s -> Splash.draw s
    Registration s -> Registration.draw s

handleEvent
  :: Ord e => Store e -> BrickEvent Name e -> EventM Name (Next (Store e))
handleEvent store ev = handle store ev
 where
  handle = if Store.handleGlobalEvent store ev
    then Global.handle
    else case store ^. state of
      Splash       screen -> Splash.handle screen
      Registration screen -> Registration.handle screen

buildVty :: IO Vty
buildVty = do
  v <- V.mkVty =<< V.standardIOConfig
  V.setMode (V.outputIface v) V.Mouse True
  return v

chooseCursor :: Store e -> [CursorLocation Name] -> Maybe (CursorLocation Name)
chooseCursor store = case store ^. state of
  Registration screen -> fromMaybe noCursor $ Registration.chooseCursor screen
  _                   -> noCursor
  where noCursor = neverShowCursor store
