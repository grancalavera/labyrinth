{-# LANGUAGE TemplateHaskell #-}

module Labyrinth.Game.Description
  ( DTile(..)
  , DGame(..)
  , gTiles
  , gGates
  , gPlayers
  , gStartPosition
  , gRows
  , gCols
  , gTreasures
  , mkTiles
  , rows
  , cols
  )
where

import           Control.Monad                  ( forM )
import           Control.Monad.State            ( StateT
                                                , evalStateT
                                                , get
                                                , liftIO
                                                , put
                                                )
import qualified Data.List.Extended            as L
import qualified Data.Map.Strict               as Map
import           Data.Maybe                     ( fromJust
                                                , isJust
                                                , fromMaybe
                                                )
import           Linear.V2                      ( V2(..) )
import           Labyrinth.Position             ( Position )
import qualified Labyrinth.Random              as Random
import           Labyrinth.Direction            ( Direction(..) )
import qualified Labyrinth.Direction           as Direction
import           Labyrinth.Goal                 ( Goal(..)
                                                , Treasure
                                                )
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
  , _gPlayers       :: Players
  , _gStartPosition :: Position
  , _gRows          :: Int
  , _gCols          :: Int
  , _gTreasures     :: [Treasure]
  } deriving (Show)
makeLenses ''DGame

type Eval a = StateT Env IO a

mkEnv :: DGame -> IO Env
mkEnv d = do
  positions <- Random.shuffle $ unknownPositions d
  goals     <- Random.shuffle $ map (`Goal` False) (d ^. gTreasures)

  return Env {_ePositions = positions, _eGoals = goals, _ePlayers = d ^. gPlayers}

unknownPositions :: DGame -> [Position]
unknownPositions d = filter (not . (`elem` known)) (derivePositions d)
  where known = map fromJust $ filter isJust $ map (^. tPosition) (d ^. gTiles)

derivePositions :: DGame -> [Position]
derivePositions d = fromMaybe [] $ do
  xs <- L.middle $ rows d
  ys <- L.middle $ cols d
  Just $ [d ^. gStartPosition] `L.union` [ V2 x y | x <- xs, y <- ys ]

mkTiles :: DGame -> IO [(Position, Tile)]
mkTiles d = do
  env <- mkEnv d
  evalStateT (eval d) env

eval :: DGame -> Eval [(Position, Tile)]
eval d = forM (d ^. gTiles) $ \tileDesc -> do
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
      []       -> return $ pure (-1) -- something went wrong, we should have enough
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

rows :: DGame -> [Int]
rows d = [0 .. (d ^. gRows - 1)]

cols :: DGame -> [Int]
cols d = [0 .. (d ^. gCols - 1)]
