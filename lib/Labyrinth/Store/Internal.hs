module Labyrinth.Store.Internal
  ( Store(..)
  , Ev
  , EventHandler
  , state
  , modal
  )
where

import           Brick
import           Lens.Micro.TH                  ( makeLenses )
import           Labyrinth.UI                   ( Name
                                                , Screen
                                                , Modal
                                                )

data Ev = Ev deriving (Show, Eq, Ord)

data Store e = Store
  { _state :: Screen e
  , _modal :: Maybe (Modal Store e)
  }
makeLenses ''Store

type EventHandler screen e
  = screen -> Store e -> BrickEvent Name e -> EventM Name (Next (Store e))
