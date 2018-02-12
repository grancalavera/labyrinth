{-# LANGUAGE TemplateHaskell #-}
module Labyrinth.Game
    ( Game (..)
    , currentPlayer
    , players
    , tiles
    , gates
    , rowSpread
    , colSpread
    , nextPlayer
    , initialGame
    , playerColorByDefaultPosition
    -- temp
    , instructions
    , makeEnv
    , eCornerGoals
    , eShuffledTiles
    , eSetGoals
    , eForkGoals
    , ePlayers
    , eColors
    , execute
    , chooseGoal
    , choosePlayer
    -- temp
    ) where

import           Data.Monoid         ((<>))
import           Control.Applicative ((<|>))
import           Control.Monad       (mapM)
import qualified Data.Map.Strict     as Map
import           Data.Map.Strict     (Map)
import           Data.Maybe          (fromMaybe)
import           Lens.Micro          ((^.), (&), (.~))
import           Lens.Micro.TH       (makeLenses)
import qualified Labyrinth           as Labyrinth
import           Labyrinth           (Position)
import qualified Labyrinth.Players   as Players
import           Labyrinth.Players   (Player(..), Color(..), Players(..))
import qualified Labyrinth.Direction as Direction
import           Labyrinth.Direction (Direction(..))
import qualified Labyrinth.Tile      as Tile
import           Labyrinth.Tile      ( Tile(..)
                                     , Terrain(..)
                                     , goal
                                     , terrain
                                     , tenants
                                     )
import           Labyrinth.Gate      (Gate(..))
import qualified Labyrinth.Goal      as Goal
import           Labyrinth.Goal      (Goal)

data Game = Game
    { _currentPlayer       :: Maybe Player
    , _currentTilePosition :: Maybe Position
    , _players             :: Players
    , _tiles               :: Map Position Tile
    , _gates               :: Map Position Gate
    , _rowSpread           :: [Int]
    , _colSpread           :: [Int]
    } deriving (Show, Eq)
makeLenses ''Game

instance Monoid Game where
  mempty = Game
    { _currentPlayer       = Nothing
    , _currentTilePosition = Nothing
    , _players             = mempty
    , _tiles               = mempty
    , _gates               = mempty
    , _rowSpread           = []
    , _colSpread           = []
    }
  l `mappend` r = Game
    { _currentPlayer       = (l ^. currentPlayer) <|> (r ^. currentPlayer)
    , _currentTilePosition = (l ^. currentTilePosition) <|> (r ^. currentTilePosition)
    , _players             = (l ^. players) <> (r ^. players)
    , _tiles               = (l ^. tiles) <> (r ^. tiles)
    , _gates               = (l ^. gates) <> (r ^. gates)
    , _rowSpread           = chooseNonEmpty (l ^. rowSpread) (r ^. rowSpread)
    , _colSpread           = chooseNonEmpty (l ^. colSpread) (r ^. colSpread)
    }

chooseNonEmpty :: [a] -> [a] -> [a]
chooseNonEmpty [] x = x
chooseNonEmpty x [] = x
chooseNonEmpty x _ = x

--------------------------------------------------------------------------------
-- games
--------------------------------------------------------------------------------

initialGame :: Players -> IO Game
initialGame pls = do
  g <- basicGame pls
  return $ g & tiles .~ (
       mempty
    <> insertPlayer (7,7) pls (g ^. tiles)
    <> insertPlayer (1,1) pls (g ^. tiles)
    <> insertPlayer (7,1) pls (g ^. tiles)
    <> insertPlayer (1,7) pls (g ^. tiles)
    )


basicGame :: Players -> IO Game
basicGame pls = do
  ps <- Labyrinth.shuffle movingPositions
  mt <- Labyrinth.shuffle movingTiles >>= mapM Tile.randomRotate
  sp <- Labyrinth.shuffle $ Players.toList pls
  ts <- Labyrinth.shuffle Goal.treasures

  let fixedTiles = Map.fromList tileList
      movingTiles' = Map.fromList $ zip (defaultCurrentTilePosition:ps) mt
      (goals1, goals2, goals3) = distribute $ map Goal.fromTreasure ts

  return $ Game
    { _players = pls
    , _currentPlayer = Just (snd (sp !! 0))
    , _rowSpread = [0..8]
    , _colSpread = [0..8]
    , _gates = Map.fromList gateList
    , _currentTilePosition = Just defaultCurrentTilePosition
    , _tiles = mempty
        <> addGoals (Labyrinth.filterByPositions fixedGoalPositions fixedTiles) goals1
        <> addGoals (filterByTerrain Corner movingTiles') goals2
        <> addGoals (filterByTerrain Fork movingTiles') goals3
        <> fixedTiles
        <> movingTiles'
    }

insertPlayer :: Position -> Players -> Map Position Tile -> Map Position Tile
insertPlayer pos pls board = fromMaybe board $ do
  c <- Map.lookup pos playerColorByDefaultPosition
  p <- Players.lookup c pls
  t <- Map.lookup pos board
  return $ Map.insert pos (t & tenants .~ [p]) board

playerColorByDefaultPosition :: Map Position Color
playerColorByDefaultPosition = Map.fromList $ zip [(1,1), (7,1), (7,7), (1,7)] Players.colors

addGoals :: Map Position Tile -> [Goal] -> Map Position Tile
addGoals m gs = Map.fromList $ map addGoal $ zip (Map.toList m) gs
  where
    addGoal :: ((Position, Tile), Goal) -> (Position, Tile)
    addGoal ((p, t), g) = (p, t & goal .~ Just g)

filterByTerrain :: Terrain -> Map Position Tile -> Map Position Tile
filterByTerrain trn = Map.filter byTerrain
  where
    byTerrain :: Tile -> Bool
    byTerrain t = trn == t ^. terrain

--------------------------------------------------------------------------------
-- players
--------------------------------------------------------------------------------

nextPlayer :: Game -> Maybe Game
nextPlayer g = do
  currP <- g ^. currentPlayer
  nextP <- Players.next currP (g ^. players)
  return $ g & currentPlayer .~ (Just nextP)

--------------------------------------------------------------------------------
-- boards
--------------------------------------------------------------------------------

defaultCurrentTilePosition :: Position
defaultCurrentTilePosition = (2,0)

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

tileList :: [(Position, Tile)]
tileList = [ ((1, 1), Tile Corner South Nothing [])
           , ((7, 1), Tile Corner West Nothing [])
           , ((1, 7), Tile Corner East Nothing [])
           , ((7, 7), Tile Corner North Nothing [])
           , ((3, 1), Tile Fork South Nothing [])
           , ((5, 1), Tile Fork South Nothing [])
           , ((1, 3), Tile Fork East Nothing [])
           , ((1, 5), Tile Fork East Nothing [])
           , ((7, 3), Tile Fork West Nothing [])
           , ((7, 5), Tile Fork West Nothing [])
           , ((3, 7), Tile Fork North Nothing [])
           , ((5, 7), Tile Fork North Nothing [])
           , ((3, 3), Tile Fork East Nothing [])
           , ((5, 3), Tile Fork South Nothing [])
           , ((3, 5), Tile Fork North Nothing [])
           , ((5, 5), Tile Fork West Nothing [])
           ]

movingTiles :: [Tile]
movingTiles = replicate 12 (Tile Path North Nothing []) ++
              replicate 16 (Tile Corner North Nothing []) ++
              replicate 6  (Tile Fork North Nothing [])

movingPositions :: [Position]
movingPositions =
  [(x,y) | x <- [2,4,6], y <- [1,3,5,7]] ++
  [(x,y) | x <- [1..7], y <- [2, 4, 6]]

fixedGoalPositions :: [Position]
fixedGoalPositions = [ (3, 1)
                     , (5, 1)
                     , (1, 3)
                     , (3, 3)
                     , (5, 3)
                     , (7, 3)
                     , (1, 5)
                     , (3, 5)
                     , (5, 5)
                     , (7, 5)
                     , (3, 7)
                     , (5, 7)
                     ]

-- tile description: know Tile or Random Tile
data TD = T (Terrain, Direction, GD, PD) | RT (GD, PD) deriving (Show)
-- goal description: Goal, No Goal, Maybe Goal
data GD = G | MG | NG deriving (Show)
data PD = P | NP deriving (Show)
type Instruction = (Position, TD)

instructions :: [Instruction]
instructions = Map.toList $ (Map.fromList
  -- first fill the rows we know
  -- row 1
  [ ((1,1), T (Corner, South, NG, P))
  , ((3,1), T (Fork, East, G, NP))
  , ((5,1), T (Fork, East, G, NP))
  , ((7,1), T (Corner, West, NG, P))
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
  , ((1,7), T (Corner, East, NG, P))
  , ((3,7), T (Fork, North, NG, NP))
  , ((5,7), T (Fork, North, NG, NP))
  , ((7,7), T (Corner, North, NG, P))
  ])
  -- then fill the rest randomly
  <> (Map.fromList $ rt (2,0) : [rt (x,y) | x <- [1..7], y <- [1..7]])
  where rt p = (p, RT (MG, NP))

distribute :: [a] -> ([a], [a], [a])
distribute gs = (x, y, z)
  where
    (x, x') = Labyrinth.halve gs
    (y, z)  = Labyrinth.halve x'

data Environment = E
  { _eShuffledTiles :: [(Terrain, Direction)]
  , _eSetGoals      :: [Maybe Goal]
  , _eCornerGoals   :: [Maybe Goal]
  , _eForkGoals     :: [Maybe Goal]
  , _eColors        :: [Color]
  , _ePlayers       :: Players
  } deriving (Show)
makeLenses ''Environment

makeEnv :: Players -> IO Environment
makeEnv plys = do
  ts <- Labyrinth.shuffle $ replicate ps Path ++
                            replicate cs Corner ++
                            replicate fs Fork
  ds <- mapM (const Direction.random) [1..(cs+fs+ps)]
  gs <- Labyrinth.shuffle $ map (Just . Goal.fromTreasure) Goal.treasures
  cl <- Labyrinth.shuffle $ Players.colors

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
    , _eColors        = cl
    , _ePlayers       = plys
    }

  where
    cs, fs, ps :: Int
    cs = 16
    fs = 6
    ps = 12

execute :: Environment -> [Instruction] -> [(Position, Tile)]
execute _ [] = []
-- known tile
execute env ((pos, (T desc)):rest) = undefined
-- random tile
execute env ((pos, (RT desc)):rest) = undefined

chooseGoal :: Environment -> Terrain -> GD -> (Environment, Maybe Goal)
chooseGoal env _ G = case (env ^. eSetGoals) of
  []     -> (env, Nothing)
  (g:gs) -> (env & eSetGoals .~ gs, g)
chooseGoal env Corner MG = case (env ^. eCornerGoals) of
  []     -> (env, Nothing)
  (g:gs) -> (env & eCornerGoals .~ gs, g)
chooseGoal env Fork MG = case (env ^. eForkGoals) of
  []     -> (env, Nothing)
  (g:gs) -> (env & eForkGoals .~ gs, g)
chooseGoal env _ _ = (env, Nothing)

choosePlayer :: Environment -> PD -> (Environment, [Player])
choosePlayer env P = case (env ^. eColors) of
  []     -> (env, [])
  (c:cs) -> (env & eColors .~ cs, fromMaybe [] $ do
    p <- Players.lookup c (env ^. ePlayers)
    return [p])
choosePlayer env _ = (env, [])
