{-# LANGUAGE TemplateHaskell #-}
module Labyrinth.Game
    ( Game (..)
    , currentPlayer
    , currentTilePosition
    , players
    , tiles
    , gates
    , rowSpread
    , colSpread
    , nextPlayer
    , initialGame
    ) where

import           Data.Monoid         ((<>))
import           Control.Monad       (mapM)
import qualified Data.Map.Strict     as Map
import           Data.Map.Strict     (Map)
import           Lens.Micro          ((^.), (&), (.~))
import           Lens.Micro.TH       (makeLenses)
import qualified Labyrinth           as Labyrinth
import           Labyrinth           (Position)
import qualified Labyrinth.Players   as Players
import           Labyrinth.Players   (Player(..), Color(..), Players(..), firstPlayer)
import qualified Labyrinth.Direction as Direction
import           Labyrinth.Direction (Direction(..))
import           Labyrinth.Tile      ( Tile(..)
                                     , Terrain(..)
                                     )
import           Labyrinth.Gate      (Gate(..))
import qualified Labyrinth.Goal      as Goal
import           Labyrinth.Goal      (Goal)

import Control.Monad.State           (StateT, evalStateT, get, put)

data Game = Game
    { _currentPlayer       :: Maybe Player
    , _currentTilePosition :: Position
    , _players             :: Players
    , _tiles               :: Map Position Tile
    , _gates               :: Map Position Gate
    , _rowSpread           :: [Int]
    , _colSpread           :: [Int]
    } deriving (Show, Eq)
makeLenses ''Game

distribute :: [a] -> ([a], [a], [a])
distribute gs = (x, y, z)
  where
    (x, x') = Labyrinth.halve gs
    (y, z)  = Labyrinth.halve x'

--------------------------------------------------------------------------------
-- players
--------------------------------------------------------------------------------

nextPlayer :: Game -> Maybe Game
nextPlayer = undefined
-- nextPlayer g = do
--   currP <- g ^. currentPlayer
--   nextP <- Players.next currP (g ^. players)
--   return $ g & currentPlayer .~ (Just nextP)

--------------------------------------------------------------------------------
-- boards
--------------------------------------------------------------------------------

gateList :: [(Position, Gate)]
gateList = [ ((2, 0), Gate South True)
            , ((4, 0), Gate South True)
            , ((6, 0), Gate South True)
            , ((0, 2), Gate East True)
            , ((0, 4), Gate East True)
            , ((0, 6), Gate East True)
            , ((8, 2), Gate West True)
            , ((8, 4), Gate West True)
            , ((8, 6), Gate West True)
            , ((2, 8), Gate North True)
            , ((4, 8), Gate North True)
            , ((6, 8), Gate North True)
            ]

--------------------------------------------------------------------------------
-- StateT based game
--------------------------------------------------------------------------------

data Env = E
  { _eShuffledTiles :: [(Terrain, Direction)]
  , _eSetGoals      :: [Maybe Goal]
  , _eCornerGoals   :: [Maybe Goal]
  , _eForkGoals     :: [Maybe Goal]
  , _ePlayers       :: Players
  } deriving (Show)
makeLenses ''Env

type Eval a = StateT Env IO a
type Instruction = (Position, TD)

-- tile description: know Tile or Random Tile
data TD = T (Terrain, Direction, GD, PD) | RT (GD, PD) deriving (Show)
-- goal description: Goal, No Goal, Maybe Goal
data GD = G | MG | NG deriving (Show)
-- player description: Yellow Player, Green Player, Blue Player, Red Player, No Player
data PD = P Color | NP deriving (Show)

eval :: [Instruction] -> Eval [(Position, Tile)]
eval = mapM (\(pos, instruction) -> case instruction of
    T desc  -> do
      t <- evalTile desc
      return $ (pos, t)
    RT desc -> do
      t <- evalRandomTile desc
      return $ (pos, t)
  )

evalTile :: (Terrain, Direction, GD, PD) -> Eval Tile
evalTile (terrain', direction', goalDesc, playerDesc) = do
  goal' <- evalGoal terrain' goalDesc
  player' <- evalPlayer playerDesc
  return $ Tile terrain' direction' goal' player'

evalRandomTile :: (GD, PD) -> Eval Tile
evalRandomTile (goalDesc, playerDesc) = do
  env <- get
  let ((terrain', direction'):ts) = env ^. eShuffledTiles
  put (env & eShuffledTiles .~ ts)
  goal' <- evalGoal terrain' goalDesc
  player' <- evalPlayer playerDesc
  return $ Tile terrain' direction' goal' player'

evalGoal :: Terrain -> GD -> Eval (Maybe Goal)
evalGoal _ NG = return Nothing
evalGoal _ G  = do
  env <- get
  case (env ^. eSetGoals) of
    []     -> return Nothing
    (g:gs) -> put (env & eSetGoals .~ gs) >> return g
evalGoal Corner MG = do
  env <- get
  case (env ^. eCornerGoals) of
    []     -> return Nothing
    (g:gs) -> put (env & eCornerGoals .~ gs) >> return g
evalGoal Fork   MG = do
  env <- get
  case (env ^. eForkGoals) of
    []     -> return Nothing
    (g:gs) -> put (env & eForkGoals .~ gs) >> return g
evalGoal _      MG = return Nothing

evalPlayer :: PD -> Eval [Player]
evalPlayer NP = return []
evalPlayer (P color') = do
  env <- get
  case (Players.lookup color' (env ^. ePlayers)) of
    Just plyr  -> return [plyr]
    Nothing -> return []

instructions :: [Instruction]
instructions = Map.toList $ (Map.fromList
  -- first fill the rows we know
  -- row 1
  [ ((1,1), T (Corner, South, NG, P Yellow))
  , ((3,1), T (Fork, East, G, NP))
  , ((5,1), T (Fork, East, G, NP))
  , ((7,1), T (Corner, West, NG, P Red))
  -- row 3
  , ((1,3), T (Fork, East, G, NP))
  , ((3,3), T (Fork, East, G, NP))
  , ((5,3), T (Fork, South, G, NP))
  , ((7,3), T (Fork, West, G, NP))
  -- row 5
  , ((1,5), T (Fork, East, G, NP))
  , ((3,5), T (Fork, North, G, NP))
  , ((5,5), T (Fork, West, G, NP))
  , ((7,5), T (Fork, West, G, NP))
  -- row 7
  , ((1,7), T (Corner, East, NG, P Green))
  , ((3,7), T (Fork, North, NG, NP))
  , ((5,7), T (Fork, North, NG, NP))
  , ((7,7), T (Corner, North, NG, P Blue))
  ])
  -- then fill the rest randomly
  -- notice the first tile is placed
  -- on a set position, this is the
  -- tile that will be played first
  <> (Map.fromList $ rt (2,0) : [rt (x,y) | x <- [1..7], y <- [1..7]])
  where rt p = (p, RT (MG, NP))

mkBoard :: Players -> IO [(Position, Tile)]
mkBoard plys = mkEnv plys >>= \ env -> evalStateT (eval instructions) env

mkEnv :: Players -> IO Env
mkEnv plys = do
  ts <- Labyrinth.shuffle $ replicate ps Path ++
                            replicate cs Corner ++
                            replicate fs Fork
  ds <- mapM (const Direction.random) [1..(cs+fs+ps)]
  gs <- Labyrinth.shuffle $ map (Just . Goal.fromTreasure) Goal.treasures

  let (sg, cg, fg) = distribute gs

  -- only 6 corners have goals so we fill the remaining 6 with Nothing
  -- so that we can just pick them one by one without worrying they
  -- might be empty, and shuffle again
  cg' <- Labyrinth.shuffle $ cg ++ replicate 6 Nothing

  return $ E
    { _eShuffledTiles = zip ts ds
    , _eSetGoals      = sg
    , _eCornerGoals   = cg'
    , _eForkGoals     = fg
    , _ePlayers       = plys
    }

  where
    cs, fs, ps :: Int
    cs = 16
    fs = 6
    ps = 12

--------------------------------------------------------------------------------
-- games
--------------------------------------------------------------------------------

initialGame :: Players -> IO Game
initialGame players' = do
  tiles'         <- mkBoard players'
  currentPlayer' <- firstPlayer players'
  return $ Game
    { _currentPlayer = currentPlayer'
    , _currentTilePosition = (2,0)
    , _players = players'
    , _gates = Map.fromList gateList
    , _tiles = Map.fromList tiles'
    , _rowSpread = [0..8]
    , _colSpread = [0..8]
    }
