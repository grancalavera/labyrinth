module Game.ConfigurationSpec where

import           Test.Hspec
import           Labyrinth.Game                 ( Player(..)
                                                , Color(..)
                                                , PlayOrder(..)
                                                )
import qualified Labyrinth.Game.Player         as P
import           Labyrinth.Game.Configuration

spec :: Spec
spec = describe "Configuration" $ do

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
    let i = flip insert
    let conf =
          initial
            `i` Player "A" Yellow First
            `i` Player "B" Red    First
            `i` Player "C" Blue   First
            `i` Player "D" Green  First
    hasEnoughPlayers conf `shouldBe` False

  it "initially all colors should be available" $ do
    let actual = availableColors initial
    actual `shouldBe` P.colors

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