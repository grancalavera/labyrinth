module Labyrinth.UI.Players
    (addPlayers) where

import qualified Data.Map         as Map
import           Data.Map         (Map)
import           Labyrinth.Player (Player(..), Color(..))

addPlayers :: IO (Map Color Player)
addPlayers = return $ Map.fromList
  [ (Yellow, Player Yellow "Yellow Player")
  , (Blue, Player Blue "Blue Player")
  , (Green, Player Green "Green Player")
  , (Red, Player Red "Red Player")
  ]
