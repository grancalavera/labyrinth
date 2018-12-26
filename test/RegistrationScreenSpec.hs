module RegistrationScreenSpec where

import           Test.Hspec
import qualified Labyrinth.Players             as Players
import           Labyrinth.Players              ( Color(..)
                                                , Player(..)
                                                )
import qualified Labyrinth.Screens.Registration
                                               as Registration
import           Data.Maybe                     ( isJust )

spec :: Spec
spec = describe "Registration" $ do

  it "should always create a `RegistrationFormOptions` from an empty `Players`"
    $ do
        let maybeOptions = Registration.optionsFromPlayers Players.empty
        (isJust maybeOptions) `shouldBe` True

  it "should never create a `RegistrationFormOptions` when all colors are taken"
    $ do
        let players =
              Players.empty
                `Players.add` Player "A" Yellow
                `Players.add` Player "B" Red
                `Players.add` Player "C" Blue
                `Players.add` Player "D" Green
            maybeOptions = Registration.optionsFromPlayers players
        (isJust maybeOptions) `shouldBe` False
