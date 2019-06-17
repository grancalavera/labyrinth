module Labyrinth.Builder.Validation
  ( mkPlayers
  , mkTreasures
  , validatePlan
  , validateUniquePositions
  , validateUniqueGatePositions
  , validateFixedTilesPositions
  , validatePositionsCount
  )
where

import           Data.Bifunctor                                     ( bimap )
import qualified Data.List.NonEmpty            as NonEmpty
import           Control.Lens                                       ( (#) )
import           Data.Validation                                    ( Validate
                                                                    , validate
                                                                    , _Success
                                                                    , _Failure
                                                                    )
import qualified Labyrinth.Game.Player         as Player
import           Labyrinth.Game.Player                              ( Players )
import           Labyrinth.Game.Position                            ( Position )
import           Labyrinth.Builder.Internal

validatePlan
  :: (Validate f, Applicative (f [BuildError])) => BuildPlan -> f [BuildError] ()
validatePlan plan =
  ()
    <$ validateFixedTilesPositions plan
    <* validatePositionsCount plan
    <* validateUniqueGatePositions plan
    <* validateUniquePositions plan

validateFixedTilesPositions
  :: (Validate f, Applicative (f [BuildError])) => BuildPlan -> f [BuildError] ()
validateFixedTilesPositions BuildPlan { buildPositions, buildBoard } =
  () <$ traverse (validateTilePosition buildPositions) buildBoard

validatePositionsCount
  :: (Validate f, Applicative (f [BuildError])) => BuildPlan -> f [BuildError] ()
validatePositionsCount BuildPlan { buildBoard, buildPositions } =
  () <$ validateSameLength TooFewPositions TooManyPositions buildBoard buildPositions

validateUniqueGatePositions
  :: (Validate f, Applicative (f [BuildError])) => BuildPlan -> f [BuildError] ()
validateUniqueGatePositions BuildPlan { buildGates } =
  () <$ validate [DuplicatedGatePositions] (hasUniqueElements . fmap fst) buildGates

mkPlayers :: Validate f => BuildPlan -> f [BuildError] Players
mkPlayers BuildPlan { minPlayers, buildPlayers } =
  validate [InvalidMinPlayers minPlayers] ((minPlayers <=) . Player.count) buildPlayers

mkTreasures
  :: (Validate f, Applicative (f [BuildError])) => BuildPlan -> f [BuildError] [Int]
mkTreasures plan@BuildPlan { buildTreasures } =
  [1 .. buildTreasures] <$ validateTreasures plan

validateUniquePositions
  :: (Validate f, Applicative (f [BuildError])) => BuildPlan -> f [BuildError] ()
validateUniquePositions BuildPlan { buildPositions } =
  () <$ validate [DuplicatedPositions] hasUniqueElements buildPositions

validateTreasures
  :: (Validate f, Applicative (f [BuildError])) => BuildPlan -> f [BuildError] ()
validateTreasures plan@BuildPlan { buildBoard, buildTreasures } =
  ()
    <$ validateTreasurePlayerRatio plan buildTreasures
    <* validateSameLength TooFewTreasures TooManyTreasures wantsTreasure treasures
 where
  treasures     = [1 .. buildTreasures]
  wantsTreasure = NonEmpty.filter hasTreasure buildBoard

validateTreasurePlayerRatio :: Validate f => BuildPlan -> Int -> f [BuildError] ()
validateTreasurePlayerRatio plan buildTreasures = either (_Failure #) (_Success #) $ do
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

validateSameLength
  :: (Foldable t, Validate f, Applicative (f [BuildError]))
  => BuildError
  -> BuildError
  -> t a
  -> t b
  -> f [BuildError] ()
validateSameLength errTooFew errTooMany expected actual =
  () <$ validate [errTooFew] (uncurry (<=)) (lengths expected actual) <* validate
    [errTooMany]
    (uncurry (>=))
    (lengths expected actual)
  where lengths l r = bimap length length (l, r)
