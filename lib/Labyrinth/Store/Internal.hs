module Labyrinth.Store.Internal
  ( Store(..)
  , State(..)
  , Ev
  , EventHandler
  , state
  , modals
  , showModal
  , nextModal
  , hideModalAnd
  )
where

import           Data.Maybe                     ( listToMaybe )
import           Brick
import           Control.Lens                  ( makeLenses )
import           Control.Lens                     ( (&)
                                                , (%~)
                                                , (^.)
                                                )
import           Labyrinth.UI                   ( Name
                                                , Modal
                                                , SplashS
                                                , SetupS
                                                , GameS
                                                , ModalCallback
                                                )

data Ev = Ev deriving (Show, Eq, Ord)

data State e = Splash SplashS
             | Setup (SetupS e)
             | Plan GameS
             | Search GameS
             | Escape GameS
             | TurnResult GameS
             | GameOver deriving (Show)

data Store e = Store
  { _state :: State e
  , _modals :: [Modal Store e]
  } deriving (Show)
makeLenses ''Store

type EventHandler s e
  = s -> Store e -> BrickEvent Name e -> EventM Name (Next (Store e))

showModal :: Store e -> Modal Store e -> EventM Name (Next (Store e))
showModal store m = continue $ store & modals %~ (m :)

hideModalAnd :: ModalCallback Store e -> ModalCallback Store e
hideModalAnd f store = f $ store & modals %~ \case
  []       -> []
  (_ : ms) -> ms

nextModal :: Store e -> Maybe (Modal Store e)
nextModal = listToMaybe . (^. modals)
