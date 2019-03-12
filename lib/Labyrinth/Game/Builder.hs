module Labyrinth.Game.Builder where

import           Linear.V2                                ( V2(..) )

import           Labyrinth.Game.Class                     ( Game )
import           Labyrinth.Game.Direction                 ( Direction(..) )
import           Labyrinth.Game.Player                    ( PlayOrder(..)
                                                          , Players
                                                          )
import           Labyrinth.Game.Position                  ( Position )

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

data TreasureInstruction = WithoutTreasure | WithTreasure deriving (Show, Eq)

data BuildCount = CornerCount Int
                | PathCount Int
                | ForkCount Int
                | GateCount Int
                | TreasureCount Int
                deriving Show

data BuildTile = BuildGate Position Direction
               | BuildHome Position Direction PlayOrder
               | BuildFixedFork Position Direction
               | BuildTreasureCorner
               | BuildTreasureFork
               | BuildPath
               | BuildFork
               deriving (Show, Eq)

data BuildMaterials = BuildMaterials
  { buildCorners   :: BuildCount
  , buildPaths     :: BuildCount
  , buildForks     :: BuildCount
  , buildTreasures :: BuildCount
  , buildPlayers   :: Players
  }

type BuildPlan = [BuildTile]
type BuildError = String

board :: BuildPlan
board =
  [ BuildGate (V2 0 2) South
    , BuildGate (V2 0 4) South
    , BuildGate (V2 0 6) South
    , BuildGate (V2 2 0) East
    , BuildGate (V2 4 0) East
    , BuildGate (V2 6 0) East
    , BuildGate (V2 2 8) West
    , BuildGate (V2 4 8) West
    , BuildGate (V2 6 8) West
    , BuildGate (V2 8 2) North
    , BuildGate (V2 8 4) North
    , BuildGate (V2 8 6) North
    , BuildHome (V2 1 1) South First
    , BuildHome (V2 1 7) West  Second
    , BuildHome (V2 7 1) East  Fourth
    , BuildHome (V2 7 7) North Third
    , BuildFixedFork (V2 1 3) South
    , BuildFixedFork (V2 1 5) South
    , BuildFixedFork (V2 3 5) South
    , BuildFixedFork (V2 3 1) East
    , BuildFixedFork (V2 1 1) East
    , BuildFixedFork (V2 3 3) East
    , BuildFixedFork (V2 3 7) West
    , BuildFixedFork (V2 5 7) West
    , BuildFixedFork (V2 5 5) West
    , BuildFixedFork (V2 7 3) North
    , BuildFixedFork (V2 7 5) North
    , BuildFixedFork (V2 5 3) North
    ]
    <> replicate 6  BuildTreasureCorner
    <> replicate 6  BuildTreasureFork
    <> replicate 10 BuildFork
    <> replicate 12 BuildPath

isFixed :: BuildTile -> Bool
isFixed (BuildGate _ _     ) = True
isFixed (BuildHome _ _ _   ) = True
isFixed (BuildFixedFork _ _) = True
isFixed _                    = False

isRandom :: BuildTile -> Bool
isRandom = not . isFixed

fixedPlan :: BuildPlan -> BuildPlan
fixedPlan = filter isFixed

randomPlan :: BuildPlan -> BuildPlan
randomPlan = filter isRandom

buildGame :: BuildMaterials -> BuildPlan -> Either [BuildError] Game
buildGame = undefined

validateBuildCount :: BuildPlan -> BuildMaterials -> Either BuildError ()
validateBuildCount = undefined

type BuildPredicate = BuildTile -> Bool

isGate :: BuildPredicate
isGate (BuildGate _ _) = True
isGate _               = False

isPath :: BuildPredicate
isPath BuildPath = True
isPath _         = False

isCorner :: BuildPredicate
isCorner (BuildHome _ _ _)   = True
isCorner BuildTreasureCorner = True
isCorner _                   = False

isFork :: BuildPredicate
isFork (BuildFixedFork _ _) = True
isFork BuildTreasureFork    = True
isFork BuildFork            = True
isFork _                    = False

count :: BuildPredicate -> BuildPlan -> Int
count predicate = length . filter predicate

countGates :: BuildPlan -> Int
countGates = count isGate

countPaths :: BuildPlan -> Int
countPaths = count isPath

countCorners :: BuildPlan -> Int
countCorners = count isCorner

countForks :: BuildPlan -> Int
countForks = count isFork
