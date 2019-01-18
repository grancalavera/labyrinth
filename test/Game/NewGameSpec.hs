module Game.NewGameSpec where
import           Test.Hspec

import qualified Data.Set                      as Set
import           Data.Set                       ( Set )
import           Linear.V2                      ( V2(..) )
import           Labyrinth.Game.Position        ( Position )
import qualified Labyrinth.Game.NewGame        as NG


spec :: Spec
spec = describe "NewGame" $ do
  let positions =
        Set.fromList
          [ V2 0 0
          , V2 0 1
          , V2 0 2
          , V2 1 0
          , V2 1 1
          , V2 1 2
          , V2 2 0
          , V2 2 1
          , V2 2 2
          ] :: Set Position
      posDescriptions =
        Set.fromList
          [ Just $ V2 0 0
          , Just $ V2 0 1
          , Just $ V2 0 2
          , Nothing
          , Nothing
          , Nothing
          , Just $ V2 2 0
          , Just $ V2 2 1
          , Just $ V2 2 2
          ] :: Set (Maybe Position)
  it "Missing possitions should be unassigned" $ do
    let actual = NG.resolveUnassignedPositions positions posDescriptions
    actual `shouldBe` Set.fromList [V2 1 0, V2 1 1, V2 1 2]
