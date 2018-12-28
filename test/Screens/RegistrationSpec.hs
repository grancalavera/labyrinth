module Screens.RegistrationSpec where

import           Test.Hspec
import           Brick.Forms                    ( formState )
import           Lens.Micro                     ( (^.) )
import           Data.Maybe                     ( fromJust
                                                , isNothing
                                                )
import           Labyrinth.Players              ( Color(..)
                                                , Player(..)
                                                )
import           Labyrinth.Screens.Registration ( register
                                                , hasEnoughPlayers
                                                , initialScreen
                                                , isFull
                                                , form
                                                )

spec :: Spec
spec = describe "Registration" $ do

  let playerA      = Player "A" Yellow
      playerB      = Player "B" Red
      playerC      = Player "C" Blue
      playerD      = Player "D" Green

      onePlayer    = initialScreen `register` playerA
      twoPlayers   = onePlayer `register` playerB
      threePlayers = twoPlayers `register` playerC
      fourPlayers  = threePlayers `register` playerD

  it "initally a registration form should have the default player" $ do
    let form'         = fromJust (initialScreen ^. form)
        defaultPlayer = formState form'
    defaultPlayer `shouldBe` Player "" Yellow

  it "registering one player should not be enough to start a game" $ do
    hasEnoughPlayers onePlayer `shouldBe` False

  it "should move to the next player after registering the first player" $ do
    let form'         = fromJust (onePlayer ^. form)
        defaultPlayer = formState form'
    defaultPlayer `shouldBe` Player "" Red

  it "adding players to the same colour should not increase the count" $ do
    let stillOnePlayer =
          initialScreen
            `register` Player "A" Yellow
            `register` Player "B" Yellow
            `register` Player "C" Yellow
            `register` Player "D" Yellow

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
