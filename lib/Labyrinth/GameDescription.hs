{-# LANGUAGE TemplateHaskell #-}

module Labyrinth.GameDescription
    ( mkTiles
    , TileDescription(..)
    , GameDescription(..)
    ) where

import           Control.Monad        (forM)
import           Control.Monad.State  (StateT, evalStateT, get, put, liftIO)
import           Data.Maybe           (isJust, fromJust, fromMaybe)
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
  , _dColor     :: Maybe Color
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

mkTiles :: GameDescription -> IO [(Position, Tile)]
mkTiles gameDesc = do
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
getPlayer tileDesc = do
  env <- get
  return $ fromMaybe [] $ do
    color  <- tileDesc ^. dColor
    player <- Players.lookup color (env ^. ePlayers)
    return [player]
