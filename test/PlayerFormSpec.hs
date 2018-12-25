module PlayerFormSpec where

import           Test.Hspec
import qualified Labyrinth.Players             as Players
import           Labyrinth.Players              ( Color(..)
                                                , Player(..)
                                                )
import qualified Labyrinth.Widgets             as Widgets
import           Data.Maybe                     ( isJust )

spec :: Spec
spec = describe "PlayerFormOptions" $ do

  it "should always create a `PlayerFormOptions` from an empty `Players`" $ do
    let maybeOptions = Widgets.playerFormOptions Players.empty
    (isJust maybeOptions) `shouldBe` True

  it "should never create a `PlayerFormOptions` when all colors are taken" $ do
    let players =
          Players.empty
            `Players.add` Player "A" Yellow
            `Players.add` Player "B" Red
            `Players.add` Player "C" Blue
            `Players.add` Player "D" Green
        maybeOptions = Widgets.playerFormOptions players
    (isJust maybeOptions) `shouldBe` False
