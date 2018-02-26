{-# LANGUAGE TemplateHaskell #-}

module Labyrinth.GameDescription
    ( mkGame
    , TileDescription(..)
    , GameDescription(..)
    ) where

import           Control.Monad        (forM)
import           Control.Monad.State  (StateT, evalStateT, get, put, liftIO)
import           Data.Maybe           (isJust, fromJust)
import           Lens.Micro.TH        (makeLenses)
import           Lens.Micro           ((^.), (.~), (&))
import qualified Labyrinth            as Labyrinth
import           Labyrinth            (Position)
import           Labyrinth.Tile       (Tile(..), Terrain(..))
import qualified Labyrinth.Direction  as Direction
import           Labyrinth.Direction  (Direction(..))
import qualified Labyrinth.Players    as Players
import           Labyrinth.Players    (Players, Player, Color(..))
import qualified Labyrinth.Goal       as Goal
import           Labyrinth.Goal       (Goal)

data TileDescription = TD
  { _dTerrain   :: Terrain
  , _dPosition  :: Maybe Position
  , _dDirection :: Maybe Direction
  , _dGoal      :: Bool
  , _dPlayer    :: Maybe Color
  } deriving (Show)
makeLenses ''TileDescription

data GameDescription = GD
  { _dTiles     :: [TileDescription]
  , _dPositions :: [Position]
  , _dPlayers   :: Players
  } deriving (Show)
makeLenses ''GameDescription

data Env = Env
  { _ePositions   :: [Position]
  , _eGoals       :: [Goal]
  , _ePlayers     :: Players
  } deriving (Show)
makeLenses ''Env

type Eval a = StateT Env IO a

mkGame :: GameDescription -> IO [(Position, Tile)]
mkGame gameDesc = do
  env <- mkEnv gameDesc
  evalStateT (eval gameDesc) env

mkEnv :: GameDescription -> IO Env
mkEnv gameDesc = do
  positions <- Labyrinth.shuffle $ unknownPositions gameDesc
  goals     <- Labyrinth.shuffle $ map Goal.fromTreasure Goal.treasures

  return $ Env
    { _ePositions = positions
    , _eGoals     = goals
    , _ePlayers   = gameDesc ^. dPlayers
    }

unknownPositions :: GameDescription -> [Position]
unknownPositions gameDesc = filter removeKnown (gameDesc ^. dPositions)
  where
    removeKnown = not . (`elem` known)
    known = map fromJust $
            filter isJust $
            map ((^. dPosition)) (gameDesc ^. dTiles)

eval :: GameDescription -> Eval [(Position, Tile)]
eval gameDesc = forM (gameDesc ^. dTiles) (\tileDesc -> do
    position  <- getPosition tileDesc
    direction <- getDirection tileDesc
    goal      <- getGoal tileDesc
    player    <- getPlayer tileDesc
    let tile = Tile (tileDesc ^. dTerrain) direction goal player
    return $ (position, tile)
  )

getPosition :: TileDescription -> Eval Position
getPosition tileDesc = case (tileDesc ^. dPosition) of
  Just position -> return position
  _             -> do
    env <- get
    case (env ^. ePositions) of
      []      -> return (-1,-1) -- something went wrong, we should have enough
      (x:xs)  -> put (env & ePositions .~ xs) >> return x

getDirection :: TileDescription -> Eval Direction
getDirection tileDesc = case (tileDesc ^. dDirection) of
  Just direction -> return direction
  _              -> do
    direction' <- liftIO (Direction.random)
    return direction'

getGoal :: TileDescription -> Eval (Maybe Goal)
getGoal tileDesc = case (tileDesc ^. dGoal) of
  False -> return Nothing
  _     -> do
    env <- get
    case (env ^. eGoals) of
      []      -> return Nothing -- something went wrong, we should have enough
      (x:xs)  -> put (env & eGoals .~ xs) >> return (Just x)

getPlayer :: TileDescription -> Eval [Player]
getPlayer tileDesc = case (tileDesc ^. dPlayer) of
  Nothing    -> return []
  Just color -> do
    env <- get
    case (Players.lookup color (env ^. ePlayers)) of
      Nothing -> return []
      Just player -> return [player]

{-
--------------------------------------------------------------------------------
-- 16 fully known tiles
--------------------------------------------------------------------------------
 1 TD Corner  (Just (1,1)) (Just South)  False (Just Yellow)
 2 TD Fork    (Just (3,1)) (Just East)   True  Nothing
 3 TD Fork    (Just (5,1)) (Just East)   True  Nothing
 4 TD Corner  (Just (7,1)) (Just West)   False (Just Red)
 5 TD Fork    (Just (1,3)) (Just East)   True  Nothing
 6 TD Fork    (Just (3,3)) (Just East)   True  Nothing
 7 TD Fork    (Just (5,3)) (Just South)  True  Nothing
 8 TD Fork    (Just (7,3)) (Just West)   True  Nothing
 9 TD Fork    (Just (1,5)) (Just East)   True  Nothing
10 TD Fork    (Just (3,5)) (Just North)  True  Nothing
11 TD Fork    (Just (5,5)) (Just West)   True  Nothing
12 TD Fork    (Just (7,5)) (Just West)   True  Nothing
13 TD Corner  (Just (1,7)) (Just East)   False (Just Green)
14 TD Fork    (Just (3,7)) (Just North)  True  Nothing
15 TD Fork    (Just (5,7)) (Just North)  True  Nothing
16 TD Corner  (Just (7,7)) (Just North)  False (Just Blue)
--------------------------------------------------------------------------------
-- 12 randomly placed paths
--------------------------------------------------------------------------------
17 TD Path    Nothing      Nothing       False Nothing
18 TD Path    Nothing      Nothing       False Nothing
19 TD Path    Nothing      Nothing       False Nothing
20 TD Path    Nothing      Nothing       False Nothing
21 TD Path    Nothing      Nothing       False Nothing
22 TD Path    Nothing      Nothing       False Nothing
23 TD Path    Nothing      Nothing       False Nothing
24 TD Path    Nothing      Nothing       False Nothing
25 TD Path    Nothing      Nothing       False Nothing
26 TD Path    Nothing      Nothing       False Nothing
27 TD Path    Nothing      Nothing       False Nothing
28 TD Path    Nothing      Nothing       False Nothing
--------------------------------------------------------------------------------
-- 6 randomly placed corners with treasures
--------------------------------------------------------------------------------
29 TD Corner  Nothing      Nothing       True  Nothing
30 TD Corner  Nothing      Nothing       True  Nothing
31 TD Corner  Nothing      Nothing       True  Nothing
32 TD Corner  Nothing      Nothing       True  Nothing
33 TD Corner  Nothing      Nothing       True  Nothing
34 TD Corner  Nothing      Nothing       True  Nothing
--------------------------------------------------------------------------------
-- 10 randomly placed corners
--------------------------------------------------------------------------------
35 TD Corner  Nothing      Nothing       False Nothing
36 TD Corner  Nothing      Nothing       False Nothing
37 TD Corner  Nothing      Nothing       False Nothing
38 TD Corner  Nothing      Nothing       False Nothing
39 TD Corner  Nothing      Nothing       False Nothing
40 TD Corner  Nothing      Nothing       False Nothing
41 TD Corner  Nothing      Nothing       False Nothing
42 TD Corner  Nothing      Nothing       False Nothing
43 TD Corner  Nothing      Nothing       False Nothing
44 TD Corner  Nothing      Nothing       False Nothing
--------------------------------------------------------------------------------
-- 6 randomly placed forks with treasures
--------------------------------------------------------------------------------
45 TD Fork    Nothing      Nothing       True  Nothing
46 TD Fork    Nothing      Nothing       True  Nothing
47 TD Fork    Nothing      Nothing       True  Nothing
48 TD Fork    Nothing      Nothing       True  Nothing
49 TD Fork    Nothing      Nothing       True  Nothing
50 TD Fork    Nothing      Nothing       True  Nothing
--------------------------------------------------------------------------------
-- algorithm
--------------------------------------------------------------------------------
1. start by listing all the positions [(x,y)|x<-[1..7], y<-[1..7]]++[(2,0)]
2. filter out the positions of the known tiles
3. shuffle the resulting positions
4. fill the blanks tile description by tile description, tearing out
   1 shuffled position at a time from the Env
-}
