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

{-
data TileDescription = TD
  { _dPosition  :: Maybe Position
  , _dTerrain   :: Terrain
  , _dDirection :: Maybe Direction
  , _dTreasure  :: Maybe Treasure
  , _dPlayer    :: Maybe Player
  }

--------------------------------------------------------------------------------
-- 16 fully known tiles
--------------------------------------------------------------------------------
 1 TD (Just (1,1)) Corner  (Just South)  False (Just Yellow)
 2 TD (Just (3,1)) Fork    (Just East)   True  Nothing
 3 TD (Just (5,1)) Fork    (Just East)   True  Nothing
 4 TD (Just (7,1)) Corner  (Just West)   False (Just Red)
 5 TD (Just (1,3)) Fork    (Just East)   True  Nothing
 6 TD (Just (3,3)) Fork    (Just East)   True  Nothing
 7 TD (Just (5,3)) Fork    (Just South)  True  Nothing
 8 TD (Just (7,3)) Fork    (Just West)   True  Nothing
 9 TD (Just (1,5)) Fork    (Just East)   True  Nothing
10 TD (Just (3,5)) Fork    (Just North)  True  Nothing
11 TD (Just (5,5)) Fork    (Just West)   True  Nothing
12 TD (Just (7,5)) Fork    (Just West)   True  Nothing
13 TD (Just (1,7)) Corner  (Just East)   False (Just Green)
14 TD (Just (3,7)) Fork    (Just North)  True  Nothing
15 TD (Just (5,7)) Fork    (Just North)  True  Nothing
16 TD (Just (7,7)) Corner  (Just North)  False (Just Blue)
--------------------------------------------------------------------------------
-- 12 randomly placed paths
--------------------------------------------------------------------------------
17 TD Nothing      Path    Nothing       False Nothing
18 TD Nothing      Path    Nothing       False Nothing
19 TD Nothing      Path    Nothing       False Nothing
20 TD Nothing      Path    Nothing       False Nothing
21 TD Nothing      Path    Nothing       False Nothing
22 TD Nothing      Path    Nothing       False Nothing
23 TD Nothing      Path    Nothing       False Nothing
24 TD Nothing      Path    Nothing       False Nothing
25 TD Nothing      Path    Nothing       False Nothing
26 TD Nothing      Path    Nothing       False Nothing
27 TD Nothing      Path    Nothing       False Nothing
28 TD Nothing      Path    Nothing       False Nothing
--------------------------------------------------------------------------------
-- 6 randomly placed corners with treasures
--------------------------------------------------------------------------------
29 TD Nothing      Corner  Nothing       True  Nothing
30 TD Nothing      Corner  Nothing       True  Nothing
31 TD Nothing      Corner  Nothing       True  Nothing
32 TD Nothing      Corner  Nothing       True  Nothing
33 TD Nothing      Corner  Nothing       True  Nothing
34 TD Nothing      Corner  Nothing       True  Nothing
--------------------------------------------------------------------------------
-- 10 randomly placed corners
--------------------------------------------------------------------------------
35 TD Nothing      Corner  Nothing       False Nothing
36 TD Nothing      Corner  Nothing       False Nothing
37 TD Nothing      Corner  Nothing       False Nothing
38 TD Nothing      Corner  Nothing       False Nothing
39 TD Nothing      Corner  Nothing       False Nothing
40 TD Nothing      Corner  Nothing       False Nothing
41 TD Nothing      Corner  Nothing       False Nothing
42 TD Nothing      Corner  Nothing       False Nothing
43 TD Nothing      Corner  Nothing       False Nothing
44 TD Nothing      Corner  Nothing       False Nothing
--------------------------------------------------------------------------------
-- 6 randomly placed forks with treasures
--------------------------------------------------------------------------------
45 TD Nothing      Fork    Nothing       True  Nothing
46 TD Nothing      Fork    Nothing       True  Nothing
47 TD Nothing      Fork    Nothing       True  Nothing
48 TD Nothing      Fork    Nothing       True  Nothing
49 TD Nothing      Fork    Nothing       True  Nothing
50 TD Nothing      Fork    Nothing       True  Nothing

1. start by listing all the positions [(x,y)|x<-[1..7], y<-[1..7]]++[(2,0)]
2. filter out the positions of the known tiles
3. shuffle the resulting positions
4. fill the blanks tile description by tile description, tearing out
   1 shuffled position at a time from the Env
-}

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
