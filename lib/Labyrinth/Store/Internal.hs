module Labyrinth.Store.Internal
  ( Store(..)
  , EventHandler
  , state
  )
where

import           Lens.Micro.TH                  ( makeLenses )
import           Labyrinth.UI                   ( ResourceName
                                                , Screen
                                                )
import           Brick

data Store e = Store
  { _state :: Screen e
  }
makeLenses ''Store

type EventHandler screen e
  =  screen
  -> Store e
  -> BrickEvent ResourceName e
  -> EventM ResourceName (Next (Store e))
