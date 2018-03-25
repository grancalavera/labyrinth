{-# LANGUAGE TemplateHaskell #-}

module Labyrinth.GameDescription
    ( mkTiles
    , TileDescription(..)
    , GameDescription(..)
    ) where

import           Control.Monad       (forM)
import           Control.Monad.State (StateT, evalStateT, get, liftIO, put)
import qualified Data.Map            as Map
import           Data.Maybe          (fromJust, isJust)
import           Labyrinth           (Position)
import qualified Labyrinth           as Labyrinth
import           Labyrinth.Direction (Direction (..))
import qualified Labyrinth.Direction as Direction
import           Labyrinth.Goal      (Goal (..))
import qualified Labyrinth.Goal      as Goal
import           Labyrinth.Players   (Color (..), Player, Players)
import qualified Labyrinth.Players   as Players
import           Labyrinth.Tile      (Terrain (..), Tile (..))
import           Lens.Micro          ((&), (.~), (^.))
import           Lens.Micro.TH       (makeLenses)

data Env = Env
  { _ePositions :: [Position]
  , _eGoals     :: [Goal]
  , _ePlayers   :: Players
  } deriving (Show)
makeLenses ''Env

data TileDescription = TD
  { _tTerrain   :: Terrain
  , _tPosition  :: Maybe Position
  , _tDirection :: Maybe Direction
  , _tGoal      :: Bool
  , _tPlayerY   :: Bool
  , _tPlayerR   :: Bool
  , _tPlayerB   :: Bool
  , _tPlayerG   :: Bool
  } deriving (Show)
makeLenses ''TileDescription

data GameDescription = BD
  { _bTiles     :: [TileDescription]
  , _bPositions :: [Position]
  , _bPlayers   :: Players
  } deriving (Show)
makeLenses ''GameDescription

type Eval a = StateT Env IO a

mkEnv :: GameDescription -> IO Env
mkEnv boardDesc = do
  positions <- Labyrinth.shuffle $ unknownPositions boardDesc
  goals     <- Labyrinth.shuffle $ map (`Goal` False) Goal.treasures

  return $ Env
    { _ePositions = positions
    , _eGoals     = goals
    , _ePlayers   = boardDesc ^. bPlayers
    }

unknownPositions :: GameDescription -> [Position]
unknownPositions boardDesc = filter (not . (`elem` known)) ps
  where
    ps = boardDesc ^. bPositions
    known = map fromJust
              $ filter isJust
              $ map ((^. tPosition)) (boardDesc ^. bTiles)

mkTiles :: GameDescription -> IO [(Position, Tile)]
mkTiles boardDesc = do
  env <- mkEnv boardDesc
  evalStateT (eval boardDesc) env

eval :: GameDescription -> Eval [(Position, Tile)]
eval boardDesc = forM (boardDesc ^. bTiles) $ \tileDesc -> do
    position  <- getPosition tileDesc
    direction <- getDirection tileDesc
    goal      <- getGoal tileDesc
    playerY   <- getPlayer (tileDesc ^. tPlayerY) Yellow
    playerR   <- getPlayer (tileDesc ^. tPlayerR) Red
    playerB   <- getPlayer (tileDesc ^. tPlayerB) Blue
    playerG   <- getPlayer (tileDesc ^. tPlayerG) Green
    return $ (position, Tile { _terrain = (tileDesc ^. tTerrain)
                             , _direction = direction
                             , _goal      = goal
                             , _playerY   = playerY
                             , _playerR   = playerR
                             , _playerB   = playerB
                             , _playerG   = playerG
                             })

getPosition :: TileDescription -> Eval Position
getPosition tileDesc = case (tileDesc ^. tPosition) of
  Just position -> return position
  _             -> do
    env <- get
    case (env ^. ePositions) of
      []     -> return (-1,-1) -- something went wrong, we should have enough
      (x:xs) -> put (env & ePositions .~ xs) >> return x

getDirection :: TileDescription -> Eval Direction
getDirection tileDesc = case (tileDesc ^. tDirection) of
  Just direction -> return direction
  _              -> liftIO (Direction.random) >>= return

getGoal :: TileDescription -> Eval (Maybe Goal)
getGoal tileDesc = case (tileDesc ^. tGoal) of
  False -> return Nothing
  _     -> do
    env <- get
    case (env ^. eGoals) of
      []     -> return Nothing -- something went wrong, we should have enough
      (x:xs) -> put (env & eGoals .~ xs) >> return (Just x)

getPlayer :: Bool -> Color -> Eval (Maybe Player)
getPlayer hasColor color = if hasColor
  then do
    env <- get
    return $ Map.lookup color $ Players.toMap (env ^. ePlayers)
  else return Nothing
