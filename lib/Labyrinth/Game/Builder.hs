module Labyrinth.Game.Builder
  ( BuildTile(..)
  , BuildPlan(..)
  , BuildError(..)
  , gameBuilder
  , mkPlayers
  , mkTreasures
  , validatePlan
  , validateUniquePositions
  , validateUniqueGatePositions
  , validateFixedTilesPositions
  , validatePositionsCount
  , defaultPlan
  , _buildBoard
  , _buildGates
  , _buildPlayers
  , _buildPositions
  , _buildTreasures
  , _minPlayers
  )
where

import           Linear.V2                                ( V2(..) )
import           Data.List.NonEmpty                       ( NonEmpty )
import qualified Data.List.NonEmpty            as NonEmpty
import           Data.Bifunctor                           ( bimap )
import           System.Random                            ( getStdGen )

import           Control.Monad.IO.Class                   ( MonadIO
                                                          , liftIO
                                                          )
import           Control.Monad.Except                     ( ExceptT
                                                          , liftEither
                                                          )
import           Control.Lens                             ( makeLensesFor
                                                          , (#)
                                                          )
import           Data.Validation                          ( Validate
                                                          , validate
                                                          , _Success
                                                          , _Failure
                                                          )

import           Labyrinth.Game.Class                     ( Game )
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

data BuildError = InvalidMinPlayers Int
                | DuplicatedPositions
                | DuplicatedGatePositions
                | UnknownTilePosition Position
                | InvalidBuildTreasures Int
                | TooManyPositions
                | TooFewPositions
                | TooManyTreasures
                | TooFewTreasures
                deriving Eq

instance Show BuildError where
  show e = case e of
    InvalidMinPlayers n -> "Error: minPlayers should be at least " <> show n
    DuplicatedPositions ->
      "Error: buildPositions should not have duplicated positions"
    DuplicatedGatePositions -> "Error: all gates should have unique positions"
    UnknownTilePosition p ->
      "Error: a tile position must exist in buildPositions, unknown position: "
        <> show p
    InvalidBuildTreasures n ->
      "Error: buildTreasures should be a multiple of " <> show n
    TooManyPositions -> "Error: too many positions given"
    TooFewPositions  -> "Error: too few positions given"
    TooManyTreasures -> "Error: too many treasures given"
    TooFewTreasures  -> "Error: too few treasures given"


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


type GameBuilder m a = ExceptT [BuildError] m a

gameBuilder :: MonadIO m => BuildPlan -> GameBuilder m ([Int], Players)
gameBuilder plan = do
  gen <- liftIO getStdGen
  liftEither $ do
    validatePlan plan
    t <- mkTreasures plan
    p <- mkPlayers plan
    return (t, p)

validatePlan
  :: (Validate f, Applicative (f [BuildError]))
  => BuildPlan
  -> f [BuildError] ()
validatePlan plan =
  ()
    <$ validateFixedTilesPositions plan
    <* validatePositionsCount plan
    <* validateUniqueGatePositions plan
    <* validateUniquePositions plan

validateFixedTilesPositions
  :: (Validate f, Applicative (f [BuildError]))
  => BuildPlan
  -> f [BuildError] ()
validateFixedTilesPositions BuildPlan { buildPositions, buildBoard } =
  () <$ traverse (validateTilePosition buildPositions) buildBoard

validatePositionsCount
  :: (Validate f, Applicative (f [BuildError]))
  => BuildPlan
  -> f [BuildError] ()
validatePositionsCount BuildPlan { buildBoard, buildPositions } =
  ()
    <$ validateSameLength TooFewPositions
                          TooManyPositions
                          buildBoard
                          buildPositions

validateUniqueGatePositions
  :: (Validate f, Applicative (f [BuildError]))
  => BuildPlan
  -> f [BuildError] ()
validateUniqueGatePositions BuildPlan { buildGates } =
  ()
    <$ validate [DuplicatedGatePositions]
                (hasUniqueElements . fmap fst)
                buildGates

mkPlayers :: Validate f => BuildPlan -> f [BuildError] Players
mkPlayers BuildPlan { minPlayers, buildPlayers } = validate
  [InvalidMinPlayers minPlayers]
  ((minPlayers <=) . Player.count)
  buildPlayers

mkTreasures
  :: (Validate f, Applicative (f [BuildError]))
  => BuildPlan
  -> f [BuildError] [Int]
mkTreasures plan@BuildPlan { buildTreasures } =
  [1 .. buildTreasures] <$ validateTreasures plan

validateUniquePositions
  :: (Validate f, Applicative (f [BuildError]))
  => BuildPlan
  -> f [BuildError] ()
validateUniquePositions BuildPlan { buildPositions } =
  () <$ validate [DuplicatedPositions] hasUniqueElements buildPositions

validateTreasures
  :: (Validate f, Applicative (f [BuildError]))
  => BuildPlan
  -> f [BuildError] ()
validateTreasures plan@BuildPlan { buildBoard, buildTreasures } =
  ()
    <$ validateTreasurePlayerRatio plan buildTreasures
    <* validateSameLength TooFewTreasures
                          TooManyTreasures
                          wantsTreasure
                          treasures
 where
  treasures     = [1 .. buildTreasures]
  wantsTreasure = NonEmpty.filter hasTreasure buildBoard

validateTreasurePlayerRatio
  :: Validate f => BuildPlan -> Int -> f [BuildError] ()
validateTreasurePlayerRatio plan buildTreasures =
  either (_Failure #) (_Success #) $ do
    pCount <- Player.count <$> mkPlayers plan
    let pMultiple = product [1 .. pCount]
    if 0 == (buildTreasures `mod` pMultiple)
      then Right ()
      else Left [InvalidBuildTreasures pMultiple]

validateTilePosition
  :: (Validate f, Applicative (f [BuildError]))
  => BuildPositions
  -> BuildTile
  -> f [BuildError] BuildTile
validateTilePosition ps t = case t of
  BuildHome p _ _            -> t <$ validatePos ps p
  BuildFixedTreasureFork p _ -> t <$ validatePos ps p
  _                          -> _Success # t

validatePos
  :: (Validate f, Applicative (f [BuildError]))
  => BuildPositions
  -> Position
  -> f [BuildError] ()
validatePos ps p = () <$ validate [UnknownTilePosition p] (`elem` ps) p

hasUniqueElements :: (Ord a) => NonEmpty a -> Bool
hasUniqueElements ne = NonEmpty.length (NonEmpty.nub ne) == NonEmpty.length ne

validateSameLength
  :: (Foldable t, Validate f, Applicative (f [BuildError]))
  => BuildError
  -> BuildError
  -> t a
  -> t b
  -> f [BuildError] ()
validateSameLength errTooFew errTooMany expected actual =
  ()
    <$ validate [errTooFew]  (uncurry (<=)) (lengths expected actual)
    <* validate [errTooMany] (uncurry (>=)) (lengths expected actual)
  where lengths l r = bimap length length (l, r)

hasTreasure :: BuildTile -> Bool
hasTreasure (BuildFixedTreasureFork _ _) = True
hasTreasure BuildTreasureCorner          = True
hasTreasure BuildTreasureFork            = True
hasTreasure _                            = False

defaultPlan :: Players -> BuildPlan
defaultPlan players = BuildPlan { buildBoard     = board
                                , buildGates     = gates
                                , buildPositions = positions
                                , buildPlayers   = players
                                , minPlayers     = 2
                                , buildTreasures = 24
                                }

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

positions :: NonEmpty Position
positions =
  NonEmpty.fromList
    $  [ V2 row col | row <- [1 .. 7], col <- [1 .. 7] ]
    <> [V2 0 2]

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
