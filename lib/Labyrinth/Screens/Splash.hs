module Labyrinth.Screens.Splash
  ( draw
  , SplashScreen(..)
  )
where

import           Brick
import           Labyrinth.Screens.Internal     ( ResourceName )


data SplashScreen = SplashScreen

draw :: SplashScreen -> [Widget ResourceName]
draw _ = [str "Welcome and instructions on how to create a game"]
