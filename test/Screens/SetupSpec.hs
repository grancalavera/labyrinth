module Screens.SetupSpec where

import           Test.Hspec
import           Lens.Micro                     ( (^.) )
import           Data.Maybe                     ( fromJust
                                                , isNothing
                                                )
import           Labyrinth.Game                 ( Color(..)
                                                , Player(..)
                                                , PlayOrder(..)
                                                )
import           Labyrinth.UI.Screen.Setup      ( register
                                                , hasEnoughPlayers
                                                , initial
                                                , form
                                                , extractPlayer
                                                )

spec :: Spec
spec = describe "Setup" $ do

  let playerA      = Player "A" Yellow First
      playerB      = Player "B" Red Second
      playerC      = Player "C" Blue Third
      playerD      = Player "D" Green Fourth

      onePlayer    = initial `register` playerA
      twoPlayers   = onePlayer `register` playerB
      threePlayers = twoPlayers `register` playerC
      fourPlayers  = threePlayers `register` playerD

  it "initally a registration form should have the default player" $ do
    let actual = fromJust $ extractPlayer initial
    actual `shouldBe` Player "" Yellow First

  it "registering one player should not be enough to start a game" $ do
    let actual = hasEnoughPlayers onePlayer
    actual `shouldBe` False

  it "should move to the next player after registering the first player" $ do
    let actual = fromJust $ extractPlayer onePlayer
    actual `shouldBe` Player "" Red Second

  it "registering the last player should not create a new form" $ do
    let screen = threePlayers `register` playerD
    isNothing (screen ^. form) `shouldBe` True

  it "registering on a full registration should not create a new form" $ do
    let screen = fourPlayers `register` playerA
    isNothing (screen ^. form) `shouldBe` True
