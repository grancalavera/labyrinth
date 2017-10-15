module GameSpec where

import Test.Hspec
import qualified Data.Map as M
import Lens.Micro ((^.))

import qualified Labyrinth.Game as Game

spec :: Spec
spec = do
  describe "A new game" $ do
    it "should begin with an empty player map" $
      (Game.initial ^. Game.players) `shouldBe` M.empty
    it "should begin without a current player" $
      (Game.initial ^. Game.currentPlayer) `shouldBe` Nothing

  -- describe "Adding players" $ do
  --   it "should be possible to add players to a new game" $



