module Labyrinth.Store.Internal
  ( Store(..)
  , State(..)
  , Ev
  , EventHandler
  , state
  , modal
  , showModal
  , hideModal
  , nextModal
  )
where

import           Data.Maybe                     ( listToMaybe )
import           Brick
import           Lens.Micro.TH                  ( makeLenses )
import           Lens.Micro                     ( (&)
                                                , (%~)
                                                , (^.)
                                                )
import           Labyrinth.UI                   ( Name
                                                , Modal
                                                , SplashS
                                                , SetupS
                                                )

data Ev = Ev deriving (Show, Eq, Ord)
data State e = Splash SplashS
             | Setup (SetupS e)
             | Plan
             | Search
             | Return
             | TurnResult
             | GameOver deriving (Show)

data Store e = Store
  { _state :: State e
  , _modal :: [Modal Store e]
  } deriving (Show)
makeLenses ''Store
type EventHandler s e
  = s -> Store e -> BrickEvent Name e -> EventM Name (Next (Store e))

showModal :: Store e -> Modal Store e -> EventM Name (Next (Store e))
showModal store m = continue $ store & modal %~ (m :)

hideModal :: Store e -> EventM Name (Next (Store e))
hideModal store = continue $ store & modal %~ \case
  []       -> []
  (_ : ms) -> ms

nextModal :: Store e -> Maybe (Modal Store e)
nextModal = listToMaybe . (^. modal)
