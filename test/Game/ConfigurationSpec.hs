module Game.ConfigurationSpec where

import           Test.Hspec
import           Labyrinth.Game.Configuration

spec :: Spec
spec = describe "Game Configuration" $ do

  let playerA      = Player "A" Yellow First
      playerB      = Player "B" Red Second
      playerC      = Player "C" Blue Third
      playerD      = Player "D" Green Fourth

      onePlayer    = playerA `insert` initial
      twoPlayers   = playerB `insert` onePlayer
      threePlayers = playerC `insert` twoPlayers
      fourPlayers  = playerD `insert` threePlayers

  it "an initial configuration should not have enough players" $ do
    let actual = hasEnoughPlayers initial
    actual `shouldBe` False

  it "one player should not be enough" $ do
    let actual = hasEnoughPlayers onePlayer
    actual `shouldBe` False

  it "two players should be enough" $ do
    let actual = hasEnoughPlayers twoPlayers
    actual `shouldBe` True

  it "three players should be enough" $ do
    let actual = hasEnoughPlayers threePlayers
    actual `shouldBe` True

  it "four players should be enough" $ do
    let actual = hasEnoughPlayers fourPlayers
    actual `shouldBe` True

  it "only a player should exist in each playing order" $ do
    let tresni = flip insert
        conf =
          initial
            `tresni` Player "A" Yellow First
            `tresni` Player "B" Red    First
            `tresni` Player "C" Blue   First
            `tresni` Player "D" Green  First
        actual = hasEnoughPlayers conf

    actual `shouldBe` False

  it "initially all colors should be available" $ do
    let actual = availableColors initial
    actual `shouldBe` colors

  it "Yellow should be taken" $ do
    let actual = availableColors onePlayer
    actual `shouldBe` [Red, Blue, Green]

  it "Yellow and Red should be taken" $ do
    let actual = availableColors twoPlayers
    actual `shouldBe` [Blue, Green]

  it "Yellow, Red and Blue should be taken" $ do
    let actual = availableColors threePlayers
    actual `shouldBe` [Green]

  it "all colors should be taken" $ do
    let actual = availableColors fourPlayers
    actual `shouldBe` []
