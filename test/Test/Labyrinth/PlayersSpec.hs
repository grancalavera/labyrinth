module Test.Labyrinth.PlayersSpec where

import           Test.Hspec
import           Data.Monoid       ((<>))
import           Labyrinth.Players (Player(..), Color(..))
import qualified Labyrinth.Players as Players

spec :: Spec
spec = do
  describe "Players" $ do
    it "should be created from a player" $ do
      let player  = Player Yellow "Foo"
          players = Players.fromPlayer player
      Players.lookupByColor Yellow players `shouldBe` Just player

    it "should replace the player on an existing `Color`" $ do
      let createdWith = Player Yellow "Foo"
          updatedWith = Player Yellow "Bar"
          players = Players.fromPlayer createdWith <> Players.fromPlayer updatedWith
      Players.lookupByColor Yellow players `shouldBe` Just updatedWith

  describe "Turns" $ do
    it "should not be allowed with 0 players" $ do
      let player = Player Yellow "Foo"
      Players.next player mempty `shouldBe` Nothing

    it "should not be allowed with 1 player" $ do
      let player  = Player Yellow "Foo"
          players = Players.fromPlayer player
      Players.next player players `shouldBe` Nothing

    it "should skip 1 non existing player" $ do
      let currentPlayer = Player Yellow "Foo"
          nextPlayer    = Player Green "Bar"
          players       = Players.fromPlayer currentPlayer <> Players.fromPlayer nextPlayer
      Players.next currentPlayer players `shouldBe` Just nextPlayer

  --   it "should skip 2 non existing players" $
  --       Players.next player1 players1And4 `shouldBe` Just player4
  --   it "should go back to first player" $
  --       Players.next player3 players1And3 `shouldBe` Just player1
  --   it "should move from Yellow to Blue" $
  --       Players.next player1 allPlayers `shouldBe` Just player2
  --   it "should move from Blue to Green" $
  --       Players.next player2 allPlayers `shouldBe` Just player3
  --   it "should move from Green to Red" $
  --       Players.next player3 allPlayers `shouldBe` Just player4
  --   it "should move from Red to Yellow" $
  --       Players.next player4 allPlayers `shouldBe` Just player1

