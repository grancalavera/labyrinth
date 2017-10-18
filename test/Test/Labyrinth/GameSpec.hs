module Test.Labyrinth.GameSpec where

import           Test.Hspec
import qualified Data.Map           as M
import           Lens.Micro         ((^.))
import           Labyrinth.Players  (Player)
import qualified Labyrinth.Game     as Game
import           Test.Labyrinth

spec :: Spec
spec = do
  describe "A new game" $ do
    it "should begin with an empty player map" $
      pending
      -- (Game.initial ^. Game.players) `shouldBe` M.empty
    it "should begin without a current player" $
      pending
      -- (Game.initial ^. Game.currentPlayer) `shouldBe` Nothing

  describe "Adding players" $ do
    it "should be possible to add players to a new game" $
      pending
      -- lookupPlayerInGame player1 (Game.addPlayer player1 Game.initial) `shouldBe` Just player1

  describe "Turns" $ do
    it "should not be allowed with 0 players" $
      pending
      -- Game.nextPlayer Game.initial `shouldBe` Nothing
    it "should not be allowed with 1 player" $
      pending
      -- Game.nextPlayer singlePlayerGame `shouldBe` Nothing
    it "should move from player 1 to player 2" $
      pending
      -- from1to2 `shouldBe` Just player2
    it "should move from player 2 to player 3" $
      pendingWith "Need to figure out how to test this"
    it "should move from player 3 to player 4" $
      pendingWith "Need to figure out how to test this"
    it "should move from player 4 to player 1" $
      pendingWith "Need to figure out how to test this"

-- from1to2 :: Maybe Player
-- from1to2 = do
--   g <- Game.nextPlayer allPlayersGame
--   g ^. Game.currentPlayer
