module Labyrinth.Store.Internal
  ( Store(..)
  , EventHandler
  , state
  , global
  )
where

import           Brick
import           Lens.Micro.TH                  ( makeLenses )
import           Labyrinth.UI                   ( Name
                                                , Screen
                                                , Global
                                                )

data Store e = Store
  { _state :: Screen e
  , _global :: Global e
  }
makeLenses ''Store

type EventHandler screen e
  = screen -> Store e -> BrickEvent Name e -> EventM Name (Next (Store e))
