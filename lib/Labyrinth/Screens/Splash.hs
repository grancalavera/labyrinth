module Labyrinth.Screens.Splash
  ( draw
  , SplashScreen
  , initialScreen
  )
where

import           Brick
import qualified Brick.Widgets.Border          as B
import qualified Brick.Widgets.Center          as C
import           Data.Text                      ( Text )

data SplashScreen = SplashScreen

draw :: SplashScreen -> [Widget n]
draw _ =
  [ C.vCenter
      $ C.hCenter
      $ B.borderWithLabel (str "Labyrinth")
      $ hLimit 50
      $ padTop (Pad 1)
      $ padLeftRight 1
      $ page
          [ "This game is a clone of Ravensburger's Labyrinth."
          , "The game can be played with two players and with up to four players."
          , "To create a new game press [Enter] and add some players."
          ]
  ]


page :: [Text] -> Widget n
page = vBox . map paragraph

paragraph :: Text -> Widget n
paragraph = padBottom (Pad 1) . txtWrap

initialScreen :: SplashScreen
initialScreen = SplashScreen
