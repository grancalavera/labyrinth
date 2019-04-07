module Game.BuilderSpec where
import           Test.Hspec

import qualified Data.Map.Strict               as Map
import qualified Data.List.NonEmpty            as NonEmpty
import           Linear.V2                                ( V2(..) )
import           Data.Validation                          ( Validation(..) )

import qualified Labyrinth.Game.Builder        as Builder
import           Labyrinth.Game.Builder                   ( BuildTile(..)
                                                          , BuildPlan(..)
                                                          , BuildError(..)
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
spec = describe "Building new games declaratively" $ do

  -- fails these validations:
  -- validatePlayers
  -- validateUniquePositions
  -- validateUniqueGatePositions
  let validPositions = NonEmpty.fromList [V2 0 0, V2 0 1, V2 0 2, V2 0 3]
      validPlayers   = Map.fromList
        [(First, Player "P1" Yellow First), (Second, Player "P2" Red Second)]
      fourValidPlayers = Map.fromList
        [ (First , Player "P1" Yellow First)
        , (Second, Player "P2" Red Second)
        , (Third , Player "P3" Green First)
        , (Fourth, Player "P4" Blue Second)
        ]
      validPlan = BuildPlan
        { buildBoard     = NonEmpty.fromList
                             [ BuildTreasureCorner
                             , BuildTreasureCorner
                             , BuildTreasureCorner
                             , BuildTreasureCorner
                             ]
        , buildGates     = NonEmpty.fromList
                             [ (V2 0 0, Cell Gate South Open)
                             , (V2 0 1, Cell Gate South Open)
                             ]
        , buildPositions = validPositions
        , buildPlayers   = validPlayers
        , minPlayers     = 2
        , buildTreasures = 4
        }

  let invalidPositions = NonEmpty.fromList [V2 0 0, V2 0 0]
      unknownPosition1 = V2 1 0
      unknownPosition2 = V2 1 1

  let invalidPlan = BuildPlan
        { buildBoard     = NonEmpty.fromList
                             [ BuildHome unknownPosition1 South First
                             , BuildHome unknownPosition2 South First
                             ]
        , buildGates     = NonEmpty.fromList
                             [ (V2 0 0, Cell Gate South Open)
                             , (V2 0 0, Cell Gate South Open)
                             ]
        , buildPositions = invalidPositions
        , buildPlayers   = mempty
        , minPlayers     = 2
        , buildTreasures = 0
        }

  let invalidPlan2 = BuildPlan
        { buildBoard     = NonEmpty.fromList
                             [ BuildHome unknownPosition1 South First
                             , BuildHome unknownPosition2 South First
                             ]
        , buildGates     = NonEmpty.fromList
                             [ (V2 0 0, Cell Gate South Open)
                             , (V2 0 0, Cell Gate South Open)
                             ]
        , buildPositions = invalidPositions
        , buildPlayers   = fourValidPlayers
        , minPlayers     = 2
        , buildTreasures = 1
        }

  let tooFewPositionsPlan = BuildPlan
        { buildBoard     = NonEmpty.fromList [BuildPath, BuildPath]
        , buildGates     = NonEmpty.fromList
                             [ (V2 0 0, Cell Gate South Open)
                             , (V2 0 0, Cell Gate South Open)
                             ]
        , buildPositions = NonEmpty.fromList [V2 0 0]
        , buildPlayers   = fourValidPlayers
        , minPlayers     = 2
        , buildTreasures = 1
        }

  let tooManyPositionsPlan = BuildPlan
        { buildBoard     = NonEmpty.fromList [BuildPath, BuildPath]
        , buildGates     = NonEmpty.fromList
                             [ (V2 0 0, Cell Gate South Open)
                             , (V2 0 0, Cell Gate South Open)
                             ]
        , buildPositions = NonEmpty.fromList [V2 0 0, V2 0 1, V2 0 2]
        , buildPlayers   = fourValidPlayers
        , minPlayers     = 2
        , buildTreasures = 1
        }

  let tooFewTreasuresPlan = BuildPlan
        { buildBoard     = NonEmpty.fromList
                             [ BuildFixedTreasureFork (V2 0 0) North
                             , BuildTreasureCorner
                             , BuildTreasureFork
                             , BuildTreasureFork
                             ]
        , buildGates     = NonEmpty.fromList
                             [ (V2 0 0, Cell Gate South Open)
                             , (V2 0 1, Cell Gate South Open)
                             ]
        , buildPositions = NonEmpty.fromList [V2 0 0, V2 0 1, V2 0 2, V2 0 3]
        , buildPlayers   = validPlayers
        , minPlayers     = 2
        , buildTreasures = 2
        }

  let tooManyTreasuresPlan = BuildPlan
        { buildBoard     = NonEmpty.fromList
                             [ BuildFixedTreasureFork (V2 0 0) North
                             , BuildTreasureCorner
                             , BuildTreasureFork
                             , BuildTreasureFork
                             ]
        , buildGates     = NonEmpty.fromList
                             [ (V2 0 0, Cell Gate South Open)
                             , (V2 0 1, Cell Gate South Open)
                             ]
        , buildPositions = NonEmpty.fromList [V2 0 0, V2 0 1, V2 0 2, V2 0 3]
        , buildPlayers   = validPlayers
        , minPlayers     = 2
        , buildTreasures = 6
        }

  -- these two tiles are not in then `invalidPlan` but they are in the `validPlan`
  let aPosition  = V2 0 1
      fixedTile1 = BuildHome aPosition South First
      fixedTile2 = BuildFixedTreasureFork aPosition South

  context "Players" $ do
    it "fails with empty players"
      $          Builder.mkPlayers invalidPlan
      `shouldBe` Failure [InvalidMinPlayers (minPlayers invalidPlan)]

    it "succeeds with enough players"
      $          Builder.mkPlayers validPlan
      `shouldBe` Success validPlayers

  context "Plan positions" $ do
    it "fails with duplicate tile positions"
      $          Builder.validateUniquePositions invalidPlan
      `shouldBe` Failure [DuplicatedPositions]

    it "succeeds with unique tile positions"
      $          Builder.validateUniquePositions validPlan
      `shouldBe` Success validPlan

    it "fails with duplicate gate positions"
      $          Builder.validateUniqueGatePositions invalidPlan
      `shouldBe` Failure [DuplicatedGatePositions]

    it "succeeds with unique gate positions"
      $          Builder.validateUniqueGatePositions validPlan
      `shouldBe` Success validPlan

  context "Fixed tile positions" $ do
    it "fails when `BuildHome` position is not in the plan" $ do
      let actual   = Builder.validateTilePosition invalidPositions fixedTile1
          expected = Failure [UnknownTilePosition aPosition]
      actual `shouldBe` expected

    it "fails when `BuildFixedTreasureFork` position is not in the plan" $ do
      let actual   = Builder.validateTilePosition invalidPositions fixedTile2
          expected = Failure [UnknownTilePosition aPosition]
      actual `shouldBe` expected

    it "suceeds when `BuildHome` position is in the plan"
      $          Builder.validateTilePosition validPositions fixedTile1
      `shouldBe` Success fixedTile1

    it "suceeds when `BuildFixedTreasureFork` position is in the plan"
      $          Builder.validateTilePosition validPositions fixedTile2
      `shouldBe` Success fixedTile2

    it "shows errors for each unknown tile position"
      $          Builder.validateFixedTilesPositions invalidPlan
      `shouldBe` Failure
                   [ UnknownTilePosition unknownPosition1
                   , UnknownTilePosition unknownPosition2
                   ]
    it "succeeds when all fixed tile positions exist"
      $          Builder.validateFixedTilesPositions validPlan
      `shouldBe` Success validPlan

  context "Treasures" $ do
    it "fails when player validation fails"
      $          Builder.mkTreasures invalidPlan
      `shouldBe` Failure [InvalidMinPlayers (minPlayers invalidPlan)]

    it "fails when treasure count validation fails"
      $          Builder.mkTreasures invalidPlan2
      `shouldBe` Failure
                   [InvalidBuildTreasures (product [1 .. 4]), TooManyTreasures]

    it "fails when there are too few treasures"
      $          Builder.mkTreasures tooFewTreasuresPlan
      `shouldBe` Failure [TooFewTreasures]

    it "fails when there are too many treasures"
      $          Builder.mkTreasures tooManyTreasuresPlan
      `shouldBe` Failure [TooManyTreasures]

    it "succeeds when player and treasure validation succeed"
      $          Builder.mkTreasures validPlan
      `shouldBe` Success [1 .. 4]

  context "Total positions count" $ do
    it "fails when there are too many positions"
      $          Builder.validatePositionsCount tooManyPositionsPlan
      `shouldBe` Failure [TooManyPositions]

    it "fails when there are too few positions"
      $          Builder.validatePositionsCount tooFewPositionsPlan
      `shouldBe` Failure [TooFewPositions]

    it "succeeds when equal to `BuildBoard` count"
      $          Builder.validatePositionsCount validPlan
      `shouldBe` Success validPositions
