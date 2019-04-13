module Game.BuilderSpec where
import           Test.Hspec

import qualified Data.Map.Strict               as Map
import qualified Data.List.NonEmpty            as NonEmpty
import           Data.List.NonEmpty                       ( NonEmpty )
import           Linear.V2                                ( V2(..) )
import           Data.Validation                          ( Validation(..) )
import           Lens.Micro                               ( (&)
                                                          , (.~)
                                                          , (^.)
                                                          )

import qualified Labyrinth.Game.Builder        as Builder
import           Labyrinth.Game.Builder                   ( BuildTile(..)
                                                          , BuildPlan(..)
                                                          , BuildError(..)
                                                          , _buildBoard
                                                          , _buildGates
                                                          , _buildPositions
                                                          )
import           Labyrinth.Game.Direction                 ( Direction(..) )
import           Labyrinth.Game.Cell                      ( GateState(..)
                                                          , Cell(..)
                                                          , Terrain(..)
                                                          )
import           Labyrinth.Game.Player                    ( Color(..)
                                                          , PlayOrder(..)
                                                          , Player(..)
                                                          , Players
                                                          )

players1, players2, players3, players4 :: Players
players1 = Map.fromList [(First, Player "P1" Yellow First)]
players2 = Map.fromList [(Second, Player "P2" Red Second)] <> players1
players3 = Map.fromList [(Third, Player "P3" Green First)] <> players2
players4 = Map.fromList [(Fourth, Player "P4" Blue Second)] <> players3

invalidPlan0Players :: BuildPlan
invalidPlan0Players = Builder.defaultPlan mempty

invalidPlan1Players :: BuildPlan
invalidPlan1Players = Builder.defaultPlan players1

validPlan2Players :: BuildPlan
validPlan2Players = Builder.defaultPlan players2

validPlan3Players :: BuildPlan
validPlan3Players = Builder.defaultPlan players3

validPlan4Players :: BuildPlan
validPlan4Players = Builder.defaultPlan players4

duplicatedBoardPositions :: BuildPlan
duplicatedBoardPositions = validPlan2Players & _buildPositions .~ addDuplicate
  (validPlan2Players ^. _buildPositions)

duplicatedGates :: BuildPlan
duplicatedGates = validPlan2Players & _buildGates .~ addDuplicate
  (validPlan2Players ^. _buildGates)

addDuplicate :: Ord a => NonEmpty a -> NonEmpty a
addDuplicate l = NonEmpty.insert (NonEmpty.head l) l

spec :: Spec
spec = describe "Building new games declaratively" $ do
  context "Players" $ do

    it "fails with empty players"
      $          Builder.mkPlayers invalidPlan0Players
      `shouldBe` Failure [InvalidMinPlayers 2]

    it "fails with empty players"
      $          Builder.mkPlayers invalidPlan1Players
      `shouldBe` Failure [InvalidMinPlayers 2]

    it "succeeds with 2 valid players"
      $          Builder.mkPlayers validPlan2Players
      `shouldBe` Success players2

    it "succeeds with 3 valid players"
      $          Builder.mkPlayers validPlan3Players
      `shouldBe` Success players3

    it "succeeds with 4 valid players"
      $          Builder.mkPlayers validPlan4Players
      `shouldBe` Success players4

  context "Board positions" $ do
    it "fails with duplicate tile positions"
      $          Builder.validateUniquePositions duplicatedBoardPositions
      `shouldBe` Failure [DuplicatedPositions]

    it "succeeds with unique tile positions"
      $          Builder.validateUniquePositions validPlan2Players
      `shouldBe` Success ()

  context "Gate positions" $ do
    it "fails with duplicate gate positions"
      $          Builder.validateUniqueGatePositions duplicatedGates
      `shouldBe` Failure [DuplicatedGatePositions]

    it "succeeds with unique gate positions"
      $          Builder.validateUniqueGatePositions validPlan2Players
      `shouldBe` Success ()

  context "Fixed tile positions" $ do
    it "fails when tiles are placed in unknown positions" $ do
      let unknownP1 = V2 100 0
          unknownP2 = V2 100 1
          t1        = BuildHome unknownP1 South First
          t2        = BuildFixedTreasureFork unknownP2 South
          plan =
            validPlan2Players
              &  _buildBoard
              .~ (validPlan2Players ^. _buildBoard <> NonEmpty.fromList [t1, t2]
                 )

      Builder.validateFixedTilesPositions plan `shouldBe` Failure
        [UnknownTilePosition unknownP1, UnknownTilePosition unknownP2]

    it "succeeds when all fixed tile positions exist"
      $          Builder.validateFixedTilesPositions validPlan2Players
      `shouldBe` Success ()

  context "Treasures" $ do
    it "fails when player validation fails"
      $          Builder.mkTreasures invalidPlan0Players
      `shouldBe` Failure [InvalidMinPlayers (minPlayers invalidPlan0Players)]

    it "fails when there are too few treasures" $ do
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
            , buildPositions = NonEmpty.fromList
                                 [V2 0 0, V2 0 1, V2 0 2, V2 0 3]
            , buildPlayers   = players2
            , minPlayers     = 2
            , buildTreasures = 2
            }
      Builder.mkTreasures tooFewTreasuresPlan
        `shouldBe` Failure [TooFewTreasures]

    it "fails when there are too many treasures" $ do
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
            , buildPositions = NonEmpty.fromList
                                 [V2 0 0, V2 0 1, V2 0 2, V2 0 3]
            , buildPlayers   = players2
            , minPlayers     = 2
            , buildTreasures = 6
            }
      Builder.mkTreasures tooManyTreasuresPlan
        `shouldBe` Failure [TooManyTreasures]

    it "succeeds with valid 2 players plan"
      $          Builder.mkTreasures validPlan2Players
      `shouldBe` Success [1 .. 24]

    it "succeeds with valid 3 players plan"
      $          Builder.mkTreasures validPlan3Players
      `shouldBe` Success [1 .. 24]

    it "succeeds with valid 4 players plan"
      $          Builder.mkTreasures validPlan4Players
      `shouldBe` Success [1 .. 24]

  context "Total positions count" $ do
    it "fails when there are too many positions" $ do
      let tooManyPositionsPlan = BuildPlan
            { buildBoard     = NonEmpty.fromList [BuildPath, BuildPath]
            , buildGates     = NonEmpty.fromList
                                 [ (V2 0 0, Cell Gate South Open)
                                 , (V2 0 0, Cell Gate South Open)
                                 ]
            , buildPositions = NonEmpty.fromList [V2 0 0, V2 0 1, V2 0 2]
            , buildPlayers   = players4
            , minPlayers     = 2
            , buildTreasures = 1
            }
      Builder.validatePositionsCount tooManyPositionsPlan
        `shouldBe` Failure [TooManyPositions]

    it "fails when there are too few positions" $ do
      let tooFewPositionsPlan = BuildPlan
            { buildBoard     = NonEmpty.fromList [BuildPath, BuildPath]
            , buildGates     = NonEmpty.fromList
                                 [ (V2 0 0, Cell Gate South Open)
                                 , (V2 0 0, Cell Gate South Open)
                                 ]
            , buildPositions = NonEmpty.fromList [V2 0 0]
            , buildPlayers   = players4
            , minPlayers     = 2
            , buildTreasures = 1
            }

      Builder.validatePositionsCount tooFewPositionsPlan
        `shouldBe` Failure [TooFewPositions]

    it "succeeds with valid 2 players plan"
      $          Builder.validatePositionsCount validPlan2Players
      `shouldBe` Success ()

    it "succeeds with valid 3 players plan"
      $          Builder.validatePositionsCount validPlan3Players
      `shouldBe` Success ()

    it "succeeds with valid 4 players plan"
      $          Builder.validatePositionsCount validPlan4Players
      `shouldBe` Success ()
