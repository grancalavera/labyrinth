module Labyrinth.Game.Builder
  ( BuildTile(..)
  , BuildPlan(..)
  , BuildError
  --
  , minPlayersError
  , uniquePositionsError
  , uniqueGatePositionsError
  --
  , validatePlayers
  , validateUniquePositions
  , validateUniqueGatePositions
  -- temp
  , gates
  , board
  , positions
  )
where

import           Linear.V2                                ( V2(..) )
import           Data.Set                                 ( Set )
import qualified Data.Set                      as Set
import           Data.List.NonEmpty                       ( NonEmpty )
import qualified Data.List.NonEmpty            as NonEmpty
import           Labyrinth.Game.Direction                 ( Direction(..) )
import qualified Labyrinth.Game.Player         as Player
import           Labyrinth.Game.Player                    ( PlayOrder(..)
                                                          , Players
                                                          )
import           Labyrinth.Game.Position                  ( Position )
import           Labyrinth.Game.Cell                      ( GateState(..)
                                                          , Cell(..)
                                                          , Terrain(..)
                                                          )

{-
  - Take a blank board of dimensions 9x9
  - Index it starting at 0

      0 1 2 3 4 5 6 7 8
    0     G   G   G
    1   H R F R F R H
    2 G R R R R R R R G
    3   F R F R F R F
    4 G R R R R R R R G
    5   F R F R F R F
    6 G R R R R R R R G
    7   H R F R F R H
    8     G   G   G

    G: Build a gate: with a direction
    H: Build a home tile: with a direction and a player key
    F: Build a fixed tile: a fork with a direction and a treasure marker
    R: Build a random tile:
        - Pick a terrain from the remaining terrains
        - Somehow choose if you need to add a treasure, keep
          in mind we don't want all the treasures to be placed
          in adjacent tiles, we'd like to distribute them
-}

type BuildBoard = NonEmpty BuildTile
type BuildGates = NonEmpty (Position, Cell GateState)
type BuildPositions = NonEmpty Position

data BuildTile = BuildHome Position Direction PlayOrder
               | BuildFixedTreasureFork Position Direction
               | BuildTreasureCorner
               | BuildTreasureFork
               | BuildPath
               | BuildFork
               deriving (Show, Eq)

data BuildPlan = BuildPlan { buildBoard     :: BuildBoard
                           , buildGates     :: BuildGates     -- validate unique keys
                           , buildPositions :: BuildPositions -- validate unique positions
                           , buildPlayers   :: Players
                           , minPlayers     :: Int
                           } deriving(Show, Eq)

type BuildError = String
-- type BuildPredicate = BuildTile -> Bool

minPlayersError :: Int -> Either BuildError BuildPlan
minPlayersError n = Left $ "Error: minPlayers should be at least " <> (show n)

validatePlayers :: BuildPlan -> Either BuildError BuildPlan
validatePlayers plan@BuildPlan { minPlayers, buildPlayers }
  | minPlayers <= Player.count buildPlayers = Right plan
  | otherwise                               = minPlayersError minPlayers

uniquePositionsError :: Either BuildError BuildPlan
uniquePositionsError = Left "Error: positions must be unique"

validateUniquePositions :: BuildPlan -> Either BuildError BuildPlan
validateUniquePositions plan@BuildPlan { buildPositions }
  | hasUniqueElements buildPositions = Right plan
  | otherwise                        = uniquePositionsError

uniqueGatePositionsError :: Either BuildError BuildPlan
uniqueGatePositionsError = Left "Error: gates must have unique positions"

validateUniqueGatePositions :: BuildPlan -> Either BuildError BuildPlan
validateUniqueGatePositions plan@BuildPlan { buildGates }
  | hasUniqueElements gatePositions = Right plan
  | otherwise                       = uniqueGatePositionsError
  where gatePositions = NonEmpty.map fst buildGates

hasUniqueElements :: (Ord a) => NonEmpty a -> Bool
hasUniqueElements ne = (countUniques ne) == NonEmpty.length ne
  where countUniques = Set.size . Set.fromList . NonEmpty.toList

-- http://hackage.haskell.org/package/validation-1/docs/Data-Validation.html#t%3aAccValidationP
-- validateBuildCount :: BuildMaterials -> BuildPlan -> Either BuildError ()
-- validateBuildCount = undefined

-- hasValidCornerCount :: BuildMaterials -> BuildPlan -> Bool
-- hasValidCornerCount = undefined

-- hasValidPathCount :: BuildMaterials -> BuildPlan -> Bool
-- hasValidPathCount = undefined

-- hasValidForkCount :: BuildMaterials -> BuildPlan -> Bool
-- hasValidForkCount = undefined

-- hasValidGateCount :: BuildMaterials -> BuildPlan -> Bool
-- hasValidGateCount = undefined

-- hasValidTreasureCount :: BuildMaterials -> BuildPlan -> Bool
-- hasValidTreasureCount = undefined

-- isFixed :: BuildTile -> Bool
-- isFixed (BuildGate _ _             ) = True
-- isFixed (BuildHome _ _ _           ) = True
-- isFixed (BuildFixedTreasureFork _ _) = True
-- isFixed _                            = False

-- fixedPlan :: BuildPlan -> BuildPlan
-- fixedPlan = filter isFixed

-- randomPlan :: BuildPlan -> BuildPlan
-- randomPlan = filter isRandom

-- buildGame :: BuildMaterials -> BuildPlan -> Either [BuildError] Game
-- buildGame = undefined

-- isRandom :: BuildPredicate
-- isRandom = not . isFixed

-- hasTreasure :: BuildPredicate
-- hasTreasure (BuildFixedTreasureFork _ _) = True
-- hasTreasure (BuildTreasureCorner       ) = True
-- hasTreasure (BuildTreasureFork         ) = True
-- hasTreasure _                            = False

-- isGate :: BuildPredicate
-- isGate (BuildGate _ _) = True
-- isGate _               = False

-- isPath :: BuildPredicate
-- isPath BuildPath = True
-- isPath _         = False

-- isCorner :: BuildPredicate
-- isCorner (BuildHome _ _ _)   = True
-- isCorner BuildTreasureCorner = True
-- isCorner _                   = False

-- isFork :: BuildPredicate
-- isFork (BuildFixedTreasureFork _ _) = True
-- isFork BuildTreasureFork            = True
-- isFork BuildFork                    = True
-- isFork _                            = False

-- count :: BuildPredicate -> BuildPlan -> Int
-- count predicate = length . filter predicate

-- countGates :: BuildPlan -> Int
-- countGates = count isGate

-- countPaths :: BuildPlan -> Int
-- countPaths = count isPath

-- countCorners :: BuildPlan -> Int
-- countCorners = count isCorner

-- countForks :: BuildPlan -> Int
-- countForks = count isFork

-- countTreasures :: BuildPlan -> Int
-- countTreasures = count hasTreasure

gates :: BuildGates
gates = NonEmpty.fromList
  [ (V2 0 2, Cell Gate South Open)
  , (V2 0 4, Cell Gate South Open)
  , (V2 0 6, Cell Gate South Open)
  , (V2 2 0, Cell Gate East Open)
  , (V2 4 0, Cell Gate East Open)
  , (V2 6 0, Cell Gate East Open)
  , (V2 2 8, Cell Gate West Open)
  , (V2 4 8, Cell Gate West Open)
  , (V2 6 8, Cell Gate West Open)
  , (V2 8 2, Cell Gate North Open)
  , (V2 8 4, Cell Gate North Open)
  , (V2 8 6, Cell Gate North Open)
  ]

board :: BuildBoard
board =
  NonEmpty.fromList
    $  [ BuildHome (V2 1 1) South First
       , BuildHome (V2 1 7) West  Second
       , BuildHome (V2 7 1) East  Fourth
       , BuildHome (V2 7 7) North Third
       , BuildFixedTreasureFork (V2 1 3) South
       , BuildFixedTreasureFork (V2 1 5) South
       , BuildFixedTreasureFork (V2 3 5) South
       , BuildFixedTreasureFork (V2 3 1) East
       , BuildFixedTreasureFork (V2 1 1) East
       , BuildFixedTreasureFork (V2 3 3) East
       , BuildFixedTreasureFork (V2 3 7) West
       , BuildFixedTreasureFork (V2 5 7) West
       , BuildFixedTreasureFork (V2 5 5) West
       , BuildFixedTreasureFork (V2 7 3) North
       , BuildFixedTreasureFork (V2 7 5) North
       , BuildFixedTreasureFork (V2 5 3) North
       ]
    <> replicate 6  BuildTreasureCorner
    <> replicate 6  BuildTreasureFork
    <> replicate 10 BuildFork
    <> replicate 12 BuildPath

positions :: Set Position
positions =
  Set.fromList $ [ V2 row col | row <- [1 .. 7], col <- [1 .. 7] ] <> [V2 0 2]
