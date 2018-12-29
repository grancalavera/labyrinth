module Labyrinth.Store.Internal
  ( State(..)
  , Store(..)
  , EventHandler
  , state
  )
where

import           Lens.Micro.TH                  ( makeLenses )
import           Labyrinth.Screens              ( RegistrationScreen
                                                , SplashScreen
                                                , ResourceName
                                                )
import           Brick

data State e = Splash SplashScreen | Registration (RegistrationScreen e)

data Store e = Store
  { _state :: State e
  }
makeLenses ''Store

type EventHandler screen e
  =  screen
  -> Store e
  -> BrickEvent ResourceName e
  -> EventM ResourceName (Next (Store e))
