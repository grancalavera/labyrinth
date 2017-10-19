module Test.Labyrinth.GameSpec where

import           Test.Hspec
import           Data.Monoid        ((<>))
import           Lens.Micro         ((^.))
import           Labyrinth.Game     (Game(..))
import qualified Labyrinth.Game     as Game
import           Labyrinth.Players  (Player(..), Color(..))

spec :: Spec
spec = do
  describe "An empty game" $ do
    let game = mempty :: Game

    it "should not have players players" $ do
      Game.playerByColor Yellow game `shouldBe` Nothing
      Game.playerByColor Blue   game `shouldBe` Nothing
      Game.playerByColor Green  game `shouldBe` Nothing
      Game.playerByColor Red    game `shouldBe` Nothing

    it "should not have current player" $ do
      game ^. Game.currentPlayer `shouldBe` Nothing

    it "should not have next player" $ do
      Game.nextPlayer game `shouldBe` Nothing

  describe "A new game" $ do
    it "should be created from a player" $ do
      let player  = Player Yellow "yellow"
          game    = Game.fromPlayer player
      Game.playerByColor Yellow game `shouldBe` Just player

    it "should be created from a currentPlayer" $ do
      let player  = Player Yellow "yellow"
          game    = Game.fromCurrentPlayer player
      game ^. Game.currentPlayer `shouldBe` Just player

  describe "When taking turns in a game" $ do
    let yellow  = Player Yellow "One"
        blue    = Player Blue "Two"
        green   = Player Green "Three"
        red     = Player Red "Four"
        game    = Game.fromPlayer yellow  <>
                  Game.fromPlayer blue    <>
                  Game.fromPlayer green   <>
                  Game.fromPlayer red

    it "blue should follow yellow" $
      pending

    it "green should follow blue" $
      pending

    it "red should follow green" $
      pending

    it "yellow should follow red" $
      pending


  -- describe "Turns" $ do
  --   it "should not be allowed with 0 players" $
  --     pending
  --   it "should not be allowed with 1 player" $
  --     pending
  --   it "should move from player 1 to player 2" $
  --     pending
  --   it "should move from player 2 to player 3" $
  --     pendingWith "Need to figure out how to test this"
  --   it "should move from player 3 to player 4" $
  --     pendingWith "Need to figure out how to test this"
  --   it "should move from player 4 to player 1" $
  --     pendingWith "Need to figure out how to test this"
