module Labyrinth.UI.Players (addPlayers, mockPlayers) where

import Data.Monoid ((<>))
import qualified Labyrinth.Players as Players
import           Labyrinth.Players (Players, Player(..), Color(..))

addPlayers :: IO Players
addPlayers = return mockPlayers

mockPlayers :: Players
mockPlayers = mempty
  <> (Players.fromPlayer $ Player Yellow "Yellow Player")
  <> (Players.fromPlayer $ Player Blue "Blue Player")
  <> (Players.fromPlayer $ Player Green "Green Player")
  <> (Players.fromPlayer $ Player Red "Red Player")
