module Screens.RegistrationSpec where

import           Test.Hspec
import           Brick.Forms                    ( formState )
import           Lens.Micro                     ( (^.) )
import           Data.Maybe                     ( fromJust
                                                , isNothing
                                                )
import           Labyrinth.Game.Players         ( Color(..)
                                                , Player(..)
                                                , PlayOrder(..)
                                                )
import           Labyrinth.UI.Screen.Registration
                                                ( register
                                                , hasEnoughPlayers
                                                , initial
                                                , isFull
                                                , form
                                                , extractForm
                                                )

spec :: Spec
spec = describe "Registration" $ do

  let playerA      = Player "A" Yellow First
      playerB      = Player "B" Red Second
      playerC      = Player "C" Blue Third
      playerD      = Player "D" Green Fourth

      onePlayer    = initial `register` playerA
      twoPlayers   = onePlayer `register` playerB
      threePlayers = twoPlayers `register` playerC
      fourPlayers  = threePlayers `register` playerD

  it "initally a registration form should have the default player" $ do
    let form'         = extractForm $ fromJust (initial ^. form)
        defaultPlayer = formState form'
    defaultPlayer `shouldBe` Player "" Yellow First

  it "registering one player should not be enough to start a game" $ do
    hasEnoughPlayers onePlayer `shouldBe` False

  it "should move to the next player after registering the first player" $ do
    let form'         = extractForm $ fromJust (onePlayer ^. form)
        defaultPlayer = formState form'
    defaultPlayer `shouldBe` Player "" Red Second

  it "adding players to the same colour should not increase the count" $ do
    let stillOnePlayer =
          initial
            `register` Player "A" Yellow First
            `register` Player "B" Yellow First
            `register` Player "C" Yellow First
            `register` Player "D" Yellow First

    hasEnoughPlayers stillOnePlayer `shouldBe` False

  it "registering two players should be enough to start a game" $ do
    hasEnoughPlayers twoPlayers `shouldBe` True

  it "registering four players should be enough to start a game" $ do
    hasEnoughPlayers fourPlayers `shouldBe` True

  it "a registration with four players should be full" $ do
    isFull fourPlayers `shouldBe` True

  it "registering the last player should not create a new form" $ do
    let screen = threePlayers `register` playerD
    isNothing (screen ^. form) `shouldBe` True

  it "registering on a full registration should not create a new form" $ do
    let screen = fourPlayers `register` playerA
    isNothing (screen ^. form) `shouldBe` True
