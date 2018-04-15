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
  , tiles
  , gates
  , firstToken
  , rows
  , cols
  , treasureMap
  )
where

import           Control.Monad                  ( forM
                                                , guard
                                                )
import           Control.Monad.State            ( StateT
                                                , evalStateT
                                                , get
                                                , liftIO
                                                , put
                                                )
import qualified Data.List.Extended            as L
import           Data.Set                       ( Set )
import qualified Data.Set                      as Set
import qualified Data.Map.Strict               as Map
import           Data.Map.Strict                ( Map )
import           Data.Maybe                     ( fromJust
                                                , isJust
                                                , fromMaybe
                                                )
import           Linear.V2                      ( V2(..) )
import           Labyrinth.Position             ( Position )
import qualified Labyrinth.Random              as Random
import           Labyrinth.Direction            ( Direction(..) )
import qualified Labyrinth.Direction           as Direction
import           Labyrinth.Treasure             ( Treasure(..)
                                                , Searching
                                                , Found
                                                )
import           Labyrinth.Players              ( Color(..)
                                                , Players
                                                )
import qualified Labyrinth.Players             as Players
import           Labyrinth.Tile                 ( Terrain(..)
                                                , Tile(..)
                                                , Tokens
                                                )
import           Labyrinth.Gate                 ( Gate(..) )
import           Lens.Micro                     ( (&)
                                                , (.~)
                                                , (^.)
                                                )
import           Lens.Micro.TH                  ( makeLenses )

data Env = Env
  { _ePositions :: [Position]
  , _eTreasures :: [Treasure]
  , _ePlayers   :: Players
  } deriving (Show)
makeLenses ''Env

data DTile = DTile
  { _tTerrain   :: Terrain
  , _tPosition  :: Maybe Position
  , _tDirection :: Maybe Direction
  , _tTreasure  :: Bool
  , _tToken     :: Maybe Color
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
  positions    <- Random.shuffle $ unknownPositions d
  treasureMap' <- Random.shuffle $ d ^. gTreasures

  return Env
    { _ePositions = positions
    , _eTreasures = treasureMap'
    , _ePlayers   = d ^. gPlayers
    }

eval :: DGame -> Eval [(Position, Tile)]
eval d = forM (d ^. gTiles) $ \tileDesc -> do
  position  <- getPosition tileDesc
  direction <- getDirection tileDesc
  treasure  <- getTreasure tileDesc
  tokens    <- getTokens tileDesc
  return
    ( position
    , Tile
      { _terrain   = tileDesc ^. tTerrain
      , _direction = direction
      , _treasure  = treasure
      , _tokens    = tokens
      }
    )

tiles :: DGame -> IO (Map Position Tile)
tiles d = do
  env    <- mkEnv d
  tiles' <- evalStateT (eval d) env
  return $ Map.fromList tiles'

gates :: DGame -> Map Position Gate
gates d = Map.fromList (d ^. gGates)

firstToken :: DGame -> IO Color
firstToken d = fromJust <$> Random.choose (colors d)

treasureMap :: DGame -> IO (Map Color (Set Searching, Set Found))
treasureMap d = do
  ts' <- Random.shuffle $ d ^. gTreasures
  let cs = colors d
      ts = L.splitEvery (length ts' `div` length cs) ts'
  return $ Map.fromList $ zipWith (\c t -> (c, (Set.fromList t, Set.empty))) cs ts

rows :: DGame -> [Int]
rows d = [0 .. (d ^. gRows - 1)]

cols :: DGame -> [Int]
cols d = [0 .. (d ^. gCols - 1)]

unknownPositions :: DGame -> [Position]
unknownPositions d = filter (not . (`elem` known)) (derivePositions d)
  where known = map fromJust $ filter isJust $ map (^. tPosition) (d ^. gTiles)

derivePositions :: DGame -> [Position]
derivePositions d = fromMaybe [] $ do
  xs <- L.middle $ rows d
  ys <- L.middle $ cols d
  Just $ [d ^. gStartPosition] `L.union` [ V2 x y | x <- xs, y <- ys ]

colors :: DGame -> [Color]
colors d = Map.keys $ Players.toMap $ d ^. gPlayers

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

getTreasure :: DTile -> Eval (Maybe Treasure)
getTreasure tileDesc = if tileDesc ^. tTreasure
  then do
    env <- get
    case env ^. eTreasures of
      []       -> return Nothing -- something went wrong, we should have enough
      (x : xs) -> put (env & eTreasures .~ xs) >> return (Just x)
  else return Nothing

getTokens :: DTile -> Eval Tokens
getTokens tileDesc = do
  env <- get
  return $ fromMaybe mempty $ do
    token <- tileDesc ^. tToken
    guard $ Map.member token (Players.toMap (env ^. ePlayers))
    return $ Set.fromList [token]
