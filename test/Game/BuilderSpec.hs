module Game.BuilderSpec where
import           Test.Hspec

import qualified Data.Map.Strict               as Map
import qualified Data.List.NonEmpty            as NonEmpty
import           Linear.V2                                ( V2(..) )

import qualified Labyrinth.Game.Builder        as Builder
import           Labyrinth.Game.Builder                   ( BuildTile(..)
                                                          , BuildPlan(..)
                                                          )
import           Labyrinth.Game.Direction                 ( Direction(..) )
import           Labyrinth.Game.Cell                      ( GateState(..)
                                                          , Cell(..)
                                                          , Terrain(..)
                                                          )
import           Labyrinth.Game.Player                    ( Color(..)
                                                          , PlayOrder(..)
                                                          , Player(..)
                                                          )

spec :: Spec
spec = describe "Builds new games declaratively" $ do

  -- fails these validations:
  -- validatePlayers
  -- validateUniquePositions
  -- validateUniqueGatePositions
  let invalidPlan = BuildPlan
        { buildBoard     = NonEmpty.fromList
                             [ BuildHome (V2 1 0) South First
                             , BuildHome (V2 1 1) South First
                             ]
        , buildGates     = NonEmpty.fromList
                             [ (V2 0 0, Cell Gate South Open)
                             , (V2 0 0, Cell Gate South Open)
                             ]
        , buildPositions = NonEmpty.fromList [V2 0 0, V2 0 0]
        , buildPlayers   = mempty
        , minPlayers     = 2
        }


  let validPlan = BuildPlan
        { buildBoard     = NonEmpty.fromList [BuildPath]
        , buildGates     = NonEmpty.fromList
                             [ (V2 0 0, Cell Gate South Open)
                             , (V2 0 1, Cell Gate South Open)
                             ]
        , buildPositions = NonEmpty.fromList [V2 0 0, V2 0 1]
        , buildPlayers   = Map.fromList
                             [ (First , Player "P1" Yellow First)
                             , (Second, Player "P2" Yellow Second)
                             ]
        , minPlayers     = 2
        }

  context "Internal validation" $ do

    it "a plan without players should fail to validate"
      $          Builder.validatePlayers invalidPlan
      `shouldBe` Builder.minPlayersError (minPlayers invalidPlan)

    it "a plan with enough players should succeed player validation"
      $          Builder.validatePlayers validPlan
      `shouldBe` Right validPlan

    it "a plan with duplicated position should fail to validate"
      $          Builder.validateUniquePositions invalidPlan
      `shouldBe` Builder.uniquePositionsError

    it "a valid plan should have unique positions"
      $          Builder.validateUniquePositions validPlan
      `shouldBe` Right validPlan

    it "a plan with gates in duplicated positions should fail to validate"
      $          Builder.validateUniqueGatePositions invalidPlan
      `shouldBe` Builder.uniqueGatePositionsError

    it "a plan with gates in unique positions should be valid"
      $          Builder.validateUniqueGatePositions validPlan
      `shouldBe` Right validPlan
