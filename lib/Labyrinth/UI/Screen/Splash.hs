module Labyrinth.UI.Screen.Splash
  ( draw
  , SplashScreen
  , initial
  , chooseCursor
  )
where

import           Brick
import qualified Brick.Widgets.Border          as B
import qualified Brick.Widgets.Center          as C
import           Labyrinth.UI.Internal
import qualified Labyrinth.UI.Widget           as Widget

data SplashScreen = SplashScreen

draw :: SplashScreen -> Widget n
draw _ =
  C.vCenter
    $ C.hCenter
    $ B.borderWithLabel (txt " Labyrinth ")
    $ hLimit 50
    $ padTop (Pad 1)
    $ padLeftRight 1
    $ Widget.page
        [ "This game is a clone of Ravensburger's Labyrinth."
        , "The game can be played with two players and with up to four players."
        , "To create a new game press [Enter] and add some players."
        ]

initial :: SplashScreen
initial = SplashScreen

chooseCursor
  :: SplashScreen
  -> Maybe ([CursorLocation Name] -> Maybe (CursorLocation Name))
chooseCursor _ = Nothing
