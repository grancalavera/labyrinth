module Test.Labyrinth where

import qualified Data.Map           as M
import           Lens.Micro         ((^.))
import           Labyrinth.Players  (Player(..), Color(..), Players, color)
import qualified Labyrinth.Players  as Players
import           Labyrinth.Game     (Game)
import qualified Labyrinth.Game     as Game

p1 :: Players
p1 = Players.addFirst player1

p1p3 :: Players
p1p3 = Players.add p1 player3

p1p4 :: Players
p1p4 = Players.add p1 player4

allPlayers :: Players
allPlayers = Players.add (Players.add p1p3 player2) player4

lookupPlayer :: Player -> Players -> Maybe Player
lookupPlayer p ps = M.lookup (p ^. color) ps

lookupPlayerInGame :: Player -> Game -> Maybe Player
lookupPlayerInGame p g = lookupPlayer p (g ^. Game.players)

replace :: Player -> Maybe Player
replace p = lookupPlayer p (Players.add p1 p)

player1, player2, player3, player4, playerA :: Player
player1 = Player Yellow "Yellow 1"
player2 = Player Blue "Blue 2"
player3 = Player Green "Green 3"
player4 = Player Red "Red 4"
playerA = Player Yellow "Yellow A"
