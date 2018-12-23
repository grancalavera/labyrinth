{-# LANGUAGE OverloadedStrings #-}

module PlayersSpec where

import           Labyrinth.Players              ( Color(..)
                                                , Player(..)
                                                )
import           Test.Hspec
import qualified Labyrinth.Players             as Players
import qualified Data.Map                      as Map
import           Lens.Micro                     ( (^.) )
import qualified Data.List                     as List

spec :: Spec
spec = describe "Adding players" $ do

  it "should add a player to an empty Players" $ do
    let player     = Player "A" Yellow
        playersMap = Map.fromList [(Yellow, player)]
        players    = Players.empty `Players.add` player
    Players.toMap players `shouldBe` playersMap

  it "1 player should not be enough" $ do
    let player  = Player "A" Yellow
        players = Players.add Players.empty player
    (players ^. Players.hasEnoughPlayers) `shouldBe` False

  it "2 players should be enough" $ do
    let players =
          Players.empty
            `Players.add` Player "A" Yellow
            `Players.add` Player "B" Blue
    (players ^. Players.hasEnoughPlayers) `shouldBe` True

  it "many players of the same color does not increase the player count" $ do
    let players =
          Players.empty
            `Players.add` Player "A" Yellow
            `Players.add` Player "B" Yellow
            `Players.add` Player "C" Yellow
            `Players.add` Player "D" Yellow
    (players ^. Players.hasEnoughPlayers) `shouldBe` False

  it "in an empty Players all colors are free" $ do
    let actual = Players.freeColors Players.empty
    actual `shouldBe` Players.colors

  it "Yellow should be free" $ do
    let actual =
          Players.freeColors
            $             Players.empty
            `Players.add` Player "A" Blue
            `Players.add` Player "B" Green
            `Players.add` Player "C" Red
    actual `shouldBe` [Yellow]

  it "Blue, Green and Red should be free" $ do
    let actual =
          Players.freeColors $ Players.empty `Players.add` Player "A" Yellow
    actual `shouldBe` (List.sort [Blue, Green, Red])

  it "Blue should be free" $ do
    let actual =
          Players.freeColors
            $             Players.empty
            `Players.add` Player "A" Yellow
            `Players.add` Player "B" Green
            `Players.add` Player "C" Red
    actual `shouldBe` [Blue]


  it "Yellow, Green and Red should be free" $ do
    let actual =
          Players.freeColors $ Players.empty `Players.add` Player "A" Blue
    actual `shouldBe` (List.sort [Yellow, Green, Red])
