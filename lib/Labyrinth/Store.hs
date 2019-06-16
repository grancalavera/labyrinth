module Labyrinth.Store
  ( Store
  , State(..)
  , Ev
  , state
  , modals
  , initial
  , isShowingModal
  , nextModal
  )
where

import           Brick
import           Control.Lens                                       ( (^.) )
import           Labyrinth.Store.Internal

import           Labyrinth.UI.Modal                                 ( Modal )
import qualified Labyrinth.UI.Modal            as UI
import qualified Labyrinth.UI.Widget           as UI
import qualified Labyrinth.UI.Screen.Setup     as SetupS

initial :: Store e
initial = Store { _state = Setup SetupS.initial, _modals = [splash] }

isShowingModal :: Store e -> Bool
isShowingModal = not . null . (^. modals)

splash :: Modal Store e
splash = UI.mkOkModal
  "welcome"
  "Begin"
  (UI.page
    [ "This game is a clone of Ravensburger's Labyrinth."
    , "The game can be played with two players and with up to four players."
    ]
  )
  (hideModalAnd continue)
