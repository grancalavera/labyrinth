module Labyrinth.Store.Internal
  ( Store(..)
  , State(..)
  , Ev
  , EventHandler
  , state
  , modal
  )
where

import           Brick
import           Lens.Micro.TH                  ( makeLenses )
import           Labyrinth.UI                   ( Name
                                                , Modal
                                                , SplashS
                                                , SetupS
                                                )

data Ev = Ev deriving (Show, Eq, Ord)
data State e = Splash SplashS | Setup (SetupS e) | Plan | Search | Return | TurnResult | GameOver
data Store e = Store
  { _state :: State e
  , _modal :: Maybe (Modal Store e)
  }
makeLenses ''Store

type EventHandler s e
  = s -> Store e -> BrickEvent Name e -> EventM Name (Next (Store e))
