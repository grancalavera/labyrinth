{-# LANGUAGE TemplateHaskell #-}

module Labyrinth.Game.Description
  ( mkTiles
  , DTile(..)
  , DGame(..)
  , gTiles
  , gGates
  , gPositions
  , gPlayers
  , gStartPosition
  , gRowMin
  , gRowMax
  , gColMin
  , gColMax
  , gTreasures
  )
where

import           Control.Monad                  ( forM )
import           Control.Monad.State            ( StateT
                                                , evalStateT
                                                , get
                                                , liftIO
                                                , put
                                                )
import qualified Data.Map.Strict               as Map
import           Data.Maybe                     ( fromJust
                                                , isJust
                                                , fromMaybe
                                                )
import           Labyrinth.Position             ( Position )
import qualified Labyrinth.Random              as Random
import           Labyrinth.Direction            ( Direction(..) )
import qualified Labyrinth.Direction           as Direction
import           Labyrinth.Goal                 ( Goal(..), Treasure )
import           Labyrinth.Players              ( Color(..)
                                                , Player
                                                , Players
                                                )
import qualified Labyrinth.Players             as Players
import           Labyrinth.Tile                 ( Terrain(..)
                                                , Tile(..)
                                                )
import           Labyrinth.Gate                 ( Gate(..) )
import           Lens.Micro                     ( (&)
                                                , (.~)
                                                , (^.)
                                                )
import           Lens.Micro.TH                  ( makeLenses )

data Env = Env
  { _ePositions :: [Position]
  , _eGoals     :: [Goal]
  , _ePlayers   :: Players
  } deriving (Show)
makeLenses ''Env

data DTile = DTile
  { _tTerrain   :: Terrain
  , _tPosition  :: Maybe Position
  , _tDirection :: Maybe Direction
  , _tGoal      :: Bool
  , _tPlayer    :: Maybe Color
  } deriving (Show)
makeLenses ''DTile

data DGame = DGame
  { _gTiles         :: [DTile]
  , _gGates         :: [(Position, Gate)]
  , _gPositions     :: [Position]
  , _gPlayers       :: Players
  , _gStartPosition :: Position
  , _gRowMin        :: Int
  , _gRowMax        :: Int
  , _gColMin        :: Int
  , _gColMax        :: Int
  , _gTreasures     :: [Treasure]
  } deriving (Show)
makeLenses ''DGame

type Eval a = StateT Env IO a

mkEnv :: DGame -> IO Env
mkEnv description = do
  positions <- Random.shuffle $ unknownPositions description
  goals     <- Random.shuffle $ map (`Goal` False) (description ^. gTreasures)

  return Env
    { _ePositions = positions
    , _eGoals     = goals
    , _ePlayers   = description ^. gPlayers
    }

unknownPositions :: DGame -> [Position]
unknownPositions description = filter (not . (`elem` known)) ps
 where
  ps = description ^. gPositions
  known =
    map fromJust $ filter isJust $ map (^. tPosition) (description ^. gTiles)

mkTiles :: DGame -> IO [(Position, Tile)]
mkTiles description = do
  env <- mkEnv description
  evalStateT (eval description) env

eval :: DGame -> Eval [(Position, Tile)]
eval description = forM (description ^. gTiles) $ \tileDesc -> do
  position  <- getPosition tileDesc
  direction <- getDirection tileDesc
  goal      <- getGoal tileDesc
  player    <- getPlayer tileDesc
  return
    ( position
    , Tile
      { _terrain   = tileDesc ^. tTerrain
      , _direction = direction
      , _goal      = goal
      , _players   = player
      }
    )

getPosition :: DTile -> Eval Position
getPosition tileDesc = case tileDesc ^. tPosition of
  Just position -> return position
  _             -> do
    env <- get
    case env ^. ePositions of
      []       -> return (-1, -1) -- something went wrong, we should have enough
      (x : xs) -> put (env & ePositions .~ xs) >> return x

getDirection :: DTile -> Eval Direction
getDirection tileDesc = case tileDesc ^. tDirection of
  Just direction -> return direction
  _              -> liftIO Direction.random

getGoal :: DTile -> Eval (Maybe Goal)
getGoal tileDesc = if tileDesc ^. tGoal
  then do
    env <- get
    case env ^. eGoals of
      []       -> return Nothing -- something went wrong, we should have enough
      (x : xs) -> put (env & eGoals .~ xs) >> return (Just x)
  else return Nothing

getPlayer :: DTile -> Eval [Player]
getPlayer tileDesc = do
  env <- get
  return $ fromMaybe [] $ do
    color <- tileDesc ^. tPlayer
    (: []) <$> Map.lookup color (Players.toMap (env ^. ePlayers))
