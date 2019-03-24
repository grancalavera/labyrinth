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
  let plan1 = BuildPlan
        { buildBoard     = NonEmpty.fromList [BuildPath]
        , buildGates     = NonEmpty.fromList
                             [ (V2 0 0, Cell Gate South Open)
                             , (V2 0 0, Cell Gate South Open)
                             ]
        , buildPositions = NonEmpty.fromList [V2 0 0, V2 0 0]
        , buildPlayers   = mempty
        , minPlayers     = 2
        }


  let plan2 = BuildPlan
        { buildBoard     = NonEmpty.fromList [BuildPath]
        , buildGates     = NonEmpty.fromList
                             [ (V2 0 0, Cell Gate South Open)
                             , (V2 0 0, Cell Gate South Open)
                             ]
        , buildPositions = NonEmpty.fromList [V2 0 0, V2 0 1]
        , buildPlayers   = Map.fromList
                             [ (First , Player "P1" Yellow First)
                             , (Second, Player "P2" Yellow Second)
                             ]
        , minPlayers     = 2
        }

  context "Low level validation" $ do

    it "a plan without players should fail to validate" $ do
      Builder.validatePlayers plan1
        `shouldBe` Builder.minPlayersError (minPlayers plan1)

    it "a plan with enough players should succeed player validation" $ do
      Builder.validatePlayers plan2 `shouldBe` Right plan2

    it "a plan with duplicated position should fail to validate" $ do
      Builder.validateUniquePositions plan1
        `shouldBe` Builder.uniquePositionsError

    it "a plan with unique positions should succeed unique positions validation"
      $ do
          Builder.validateUniquePositions plan2 `shouldBe` Right plan2
