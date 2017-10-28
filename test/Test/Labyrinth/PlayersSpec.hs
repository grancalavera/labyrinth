module Test.Labyrinth.PlayersSpec where

import           Test.Hspec
import           Test.QuickCheck
import           Data.Monoid        ((<>))
import           Labyrinth.Players  (Player(..), Color(..), Players, Name)
import qualified Labyrinth.Players  as Players
import           Control.Monad      (replicateM)
import           Data.List          (intercalate)

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

  describe "monoid laws" $ do
    it "left identity" $ property prop_leftIdentity
    it "right identity" $ property prop_rightIdentity
    it "associativity" $ property prop_associativity

prop_leftIdentity :: Players -> Bool
prop_leftIdentity p = p <> mempty == p

prop_rightIdentity :: Players -> Bool
prop_rightIdentity p = mempty <> p == p

prop_associativity :: Players -> Players -> Players -> Bool
prop_associativity x y z = (x <> y) <> z == x <> (y <> z)

genChar :: Gen Char
genChar = arbitrary

genColor :: Gen Color
genColor = elements [Yellow, Blue, Green, Red]

genNamePart :: Gen Name
genNamePart = do
  i <- choose (1, 10)
  replicateM i genChar

genName :: Gen Name
genName = do
  i     <- choose (1, 4)
  parts <- replicateM i genNamePart
  return $ intercalate " "  parts

genPlayer :: Gen Player
genPlayer = do
  color <- genColor
  name  <- genName
  return $ Player color name

genPlayers :: Gen [Player]
genPlayers = do
  i <- choose (1, 4)
  replicateM i genPlayer

instance Arbitrary Players where
  arbitrary = do
    players <- genPlayers
    return $ foldl (<>) mempty $ map Players.fromPlayer players
