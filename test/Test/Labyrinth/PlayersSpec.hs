module Test.Labyrinth.PlayersSpec where

import           Test.Hspec
import qualified Data.Map           as M
import qualified Labyrinth.Players  as Players
import           Test.Labyrinth

spec :: Spec
spec = do
  describe "Players" $ do
    it "should be initially empty" $
        Players.initial `shouldBe`  M.empty
    it "should add a new player" $
        lookupPlayer player1 (Players.addFirst player1) `shouldBe` Just player1
    it "should replace an existing player" $
        replace playerA `shouldBe` Just playerA

  describe "Turns" $ do
    it "should not be allowed with 0 players" $
        Players.next player1 Players.initial `shouldBe` Nothing
    it "should not be allowed with 1 player" $
        Players.next player1 singletonPlayers `shouldBe` Nothing
    it "should skip 1 non existing player" $
        Players.next player1 players1And3 `shouldBe` Just player3
    it "should skip 2 non existing players" $
        Players.next player1 players1And4 `shouldBe` Just player4
    it "should go back to first player" $
        Players.next player3 players1And3 `shouldBe` Just player1
    it "should move from Yellow to Blue" $
        Players.next player1 allPlayers `shouldBe` Just player2
    it "should move from Blue to Green" $
        Players.next player2 allPlayers `shouldBe` Just player3
    it "should move from Green to Red" $
        Players.next player3 allPlayers `shouldBe` Just player4
    it "should move from Red to Yellow" $
        Players.next player4 allPlayers `shouldBe` Just player1

