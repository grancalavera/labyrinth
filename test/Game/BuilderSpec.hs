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

  let plan1 = BuildPlan
        { buildBoard     = NonEmpty.fromList [BuildPath]
        , buildGates     = NonEmpty.fromList
                             [ (V2 0 0, Cell Gate South Open)
                             , (V2 0 0, Cell Gate South Open)
                             ]
        , buildPositions = NonEmpty.fromList [V2 0 0]
        , buildPlayers   = mempty
        , minPlayers     = 2
        }


  let plan2 = BuildPlan
        { buildBoard     = NonEmpty.fromList [BuildPath]
        , buildGates     = NonEmpty.fromList
                             [ (V2 0 0, Cell Gate South Open)
                             , (V2 0 0, Cell Gate South Open)
                             ]
        , buildPositions = NonEmpty.fromList [V2 0 0]
        , buildPlayers   = Map.fromList
                             [ (First , Player "P1" Yellow First)
                             , (Second, Player "P2" Yellow Second)
                             ]
        , minPlayers     = 2
        }

  context "Low level validation" $ do

    it "a plan without players should fail to validate" $ do
      let actual   = Builder.validatePlayers plan1
          expected = Builder.minPlayersError (minPlayers plan1)
      actual `shouldBe` expected

    it "a plan with enough players should succeed player validation" $ do
      let actual   = Builder.validatePlayers plan2
          expected = Right plan2
      actual `shouldBe` expected
