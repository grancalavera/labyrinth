module Test.Labyrinth where

import qualified Data.Map           as M
import           Lens.Micro         ((^.), (&), (.~))
import           Labyrinth.Players  (Player(..), Color(..), Players(..), color)
import qualified Labyrinth.Players  as Players
-- import           Labyrinth.Game     (Game(..), currentPlayer)
-- import qualified Labyrinth.Game     as Game

-- singlePlayerGame :: Game
-- singlePlayerGame = Game.addPlayer player1 Game.initial

-- allPlayersGame :: Game
-- allPlayersGame = g & currentPlayer .~ (Just player1)
--   where
--     g = Game.addPlayer player1
--       $ Game.addPlayer player2
--       $ Game.addPlayer player3
--       $ Game.addPlayer player4 Game.initial

-- singletonPlayers :: Players
-- singletonPlayers = Players.addFirst player1

-- players1And3 :: Players
-- players1And3 = Players.add player1 $ Players.addFirst player3

-- players1And4 :: Players
-- players1And4 = Players.add player1 $ Players.addFirst player4

-- allPlayers :: Players
-- allPlayers = Players.add player1
--   $ Players.add player2
--   $ Players.add player3
--   $ Players.addFirst player4

lookupPlayer :: Player -> Players -> Maybe Player
lookupPlayer p (Players ps) = M.lookup (p ^. color) ps

-- lookupPlayerInGame :: Player -> Game -> Maybe Player
-- lookupPlayerInGame p g = lookupPlayer p (g ^. Game.players)

-- replace :: Player -> Maybe Player
-- replace p = lookupPlayer p $ Players.add p $ Players.addFirst player1

player1, player2, player3, player4, playerA :: Player
player1 = Player Yellow "Yellow 1"
player2 = Player Blue "Blue 2"
player3 = Player Green "Green 3"
player4 = Player Red "Red 4"
playerA = Player Yellow "Yellow A"
