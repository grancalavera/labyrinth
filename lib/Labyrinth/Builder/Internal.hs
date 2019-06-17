module Labyrinth.Builder.Internal where

import qualified Data.List.NonEmpty            as NonEmpty
import           Data.List.NonEmpty                                 ( NonEmpty )
import           Control.Lens                                       ( makeLensesFor )
import           Labyrinth.Game.Position                            ( Position )
import           Labyrinth.Game.Cell                                ( GateState(..)
                                                                    , Cell(..)
                                                                    )
import           Labyrinth.Game.Direction                           ( Direction(..) )
import           Labyrinth.Game.Player                              ( PlayOrder(..)
                                                                    , Players
                                                                    )

type BuildBoard = NonEmpty BuildTile
type BuildGates = NonEmpty (Position, Cell GateState)
type BuildPositions = NonEmpty Position

data BuildError = InvalidMinPlayers Int
                | DuplicatedPositions
                | DuplicatedGatePositions
                | UnknownTilePosition Position
                | InvalidBuildTreasures Int
                | TooManyPositions
                | TooFewPositions
                | TooManyTreasures
                | TooFewTreasures
                deriving (Eq, Show)

data BuildTile = BuildHome Position Direction PlayOrder
               | BuildFixedTreasureFork Position Direction
               | BuildTreasureCorner
               | BuildTreasureFork
               | BuildPath
               | BuildFork
               deriving (Show, Eq, Ord)

data BuildPlan = BuildPlan { buildBoard     :: BuildBoard
                           , buildGates     :: BuildGates
                           , buildPlayers   :: Players
                           , buildPositions :: BuildPositions
                           , buildTreasures :: Int
                           , minPlayers     :: Int
                           } deriving(Show, Eq)

makeLensesFor
  [ ("buildBoard", "_buildBoard")
  , ("buildGates", "_buildGates")
  , ("buildPlayers", "_buildPlayers")
  , ("buildPositions", "_buildPositions")
  , ("buildTreasures", "_buildTreasures")
  , ("minPlayers", "_minPlayers")
  ] ''BuildPlan

hasUniqueElements :: (Ord a) => NonEmpty a -> Bool
hasUniqueElements ne = NonEmpty.length (NonEmpty.nub ne) == NonEmpty.length ne

hasTreasure :: BuildTile -> Bool
hasTreasure (BuildFixedTreasureFork _ _) = True
hasTreasure BuildTreasureCorner          = True
hasTreasure BuildTreasureFork            = True
hasTreasure _                            = False
