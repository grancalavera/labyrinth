module Test.Labyrinth.PlayersSpec where

import           Test.Hspec
import           Data.Monoid       ((<>))
import           Labyrinth.Players (Player(..), Color(..))
import qualified Labyrinth.Players as Players

spec :: Spec
spec = do

  describe "Players" $ do
    it "should be created from a player" $ do
      let player  = Player Yellow "yellow"
          players = Players.fromPlayer player
      Players.lookupByColor Yellow players `shouldBe` Just player

    it "should replace the player on an existing `Color`" $ do
      let createdWith = Player Yellow "yellow 1"
          updatedWith = Player Yellow "yellow 2"
          players = Players.fromPlayer createdWith <>
                    Players.fromPlayer updatedWith
      Players.lookupByColor Yellow players `shouldBe` Just updatedWith

  describe "Turns" $ do
    context "when there are less than 2 players" $ do
      it "should not be allowed with 0 players" $ do
        let player = Player Yellow "yellow"
        Players.next player mempty `shouldBe` Nothing

      it "should not be allowed with 1 player" $ do
        let player  = Player Yellow "yellow"
            players = Players.fromPlayer player
        Players.next player players `shouldBe` Nothing

      it "should skip 1 non existing player" $ do
        let currentPlayer = Player Yellow "yellow"
            nextPlayer    = Player Green "green"
            players       = Players.fromPlayer currentPlayer <>
                            Players.fromPlayer nextPlayer
        Players.next currentPlayer players `shouldBe` Just nextPlayer

      it "should skip 2 non existing players" $ do
        let currentPlayer = Player Yellow "yellow"
            nextPlayer    = Player Red "red"
            players       = Players.fromPlayer currentPlayer <>
                            Players.fromPlayer nextPlayer
        Players.next currentPlayer players `shouldBe` Just nextPlayer

    context "when there are 4 players" $ do
      let yellow  = Player Yellow "One"
          blue    = Player Blue "Two"
          green   = Player Green "Three"
          red     = Player Red "Four"
          players = Players.fromPlayer yellow <>
                    Players.fromPlayer blue   <>
                    Players.fromPlayer green  <>
                    Players.fromPlayer red

      it "blue should follow yellow" $
        Players.next yellow players `shouldBe` Just blue

      it "green should follow blue" $
        Players.next blue players `shouldBe` Just green

      it "red should follow green" $
        Players.next green players `shouldBe` Just red

      it "yellow should follow red" $
        Players.next red players `shouldBe` Just yellow
