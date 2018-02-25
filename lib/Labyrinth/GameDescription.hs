{-# LANGUAGE TemplateHaskell #-}

module Labyrinth.GameDescription
    ( mkGame
    , GameDescription(..)
    , TileDescription(..)
    , GoalDescription(..)
    , dTiles
    , dPlayers

    -- temp
    , dPosition
    , dTerrain
    , dDirection
    , dPlayer
    , dGoal

    , eTerrains
    , eSetGoals
    , eForkGoals
    , eCornerGoals
    , ePlayers

    , mkEnv
    , eval
    , evalTerrain
    , evalDirection
    , evalGoal
    -- temp
    ) where

import           Control.Monad        (forM)
import           Control.Monad.State  (StateT, evalStateT, get, put)
import           Lens.Micro.TH        (makeLenses)
import           Lens.Micro           ((^.), (.~), (&), (%~))
import qualified Labyrinth            as Labyrinth
import           Labyrinth            (Position)
import           Labyrinth.Tile       (Tile(..), Terrain(..))
import           Labyrinth.Direction  (Direction(..))
import           Labyrinth.Players    (Players, Player, Color(..))
import qualified Labyrinth.Goal       as Goal
import           Labyrinth.Goal       (Treasure(..), Goal)

--------------------------------------------------------------------------------
-- A
--------------------------------------------------------------------------------

data GoalDescription = AddGoal | MaybeGoal | NoGoal deriving (Show)

data TileDescription = TD
  { _dPosition  :: Position
  , _dTerrain   :: Maybe Terrain
  , _dDirection :: Maybe Direction
  , _dGoal      :: GoalDescription
  , _dPlayer    :: Maybe Color
  } deriving (Show)
makeLenses ''TileDescription

data GameDescription = GD
  { _dTiles             :: [TileDescription]
  , _dPlayers           :: Players
  } deriving (Show)
makeLenses ''GameDescription

data Env = Env
  { _eTerrains    :: [Terrain]
  , _eSetGoals    :: [Maybe Goal]
  , _eForkGoals   :: [Maybe Goal]
  , _eCornerGoals :: [Maybe Goal]
  , _ePlayers     :: Players
  } deriving (Show)
makeLenses ''Env

type Eval a = StateT Env IO a

--------------------------------------------------------------------------------
-- B
--------------------------------------------------------------------------------

-- 24 goals:
-- 12 set goals
-- 6 corner goals
-- 6 fork goals

mkGame :: GameDescription -> IO [(Position, Tile)]
mkGame gameDesc = do
  env <- mkEnv gameDesc
  evalStateT (eval gameDesc) env

mkEnv :: GameDescription -> IO Env
mkEnv gameDesc = do
  terrains <- Labyrinth.shuffle $ replicate shuffledPaths   Path   ++
                                  replicate shuffledCorners Corner ++
                                  replicate shuffledForks   Fork
  goals    <- Labyrinth.shuffle $ map (Just . Goal.fromTreasure) Goal.treasures

  let (setGoals, cornerGoals', forkGoals) = distribute goals
  -- there aren't enough corner goals
  cornerGoals <- Labyrinth.shuffle $ cornerGoals' ++ (replicate 6 Nothing)

  return $ Env
    { _eTerrains    = terrains
    , _eSetGoals    = setGoals
    , _eCornerGoals = cornerGoals
    , _eForkGoals   = forkGoals
    , _ePlayers     = gameDesc ^. dPlayers
    }

  where
    shuffledPaths   = 12
    shuffledCorners = 16
    shuffledForks   = 6
    distribute goals' = (x, y, z)
      where
        (x, x') = Labyrinth.halve goals'
        (y, z)  = Labyrinth.halve x'

eval :: GameDescription -> Eval [(Position, Tile)]
eval gameDesc = forM (gameDesc ^. dTiles) (\tileDesc -> do
    terrain'    <- evalTerrain tileDesc
    direction'  <- evalDirection tileDesc
    goal'       <- evalGoal terrain' tileDesc
    player'     <- evalPlayer tileDesc
    return $ (tileDesc ^. dPosition, Tile terrain' direction' goal' player')
  )

evalTerrain :: TileDescription -> Eval Terrain
evalTerrain = undefined

evalDirection :: TileDescription -> Eval Direction
evalDirection = undefined

evalGoal :: Terrain -> TileDescription -> Eval (Maybe Goal)
evalGoal = undefined

evalPlayer :: TileDescription -> Eval [Player]
evalPlayer = undefined
