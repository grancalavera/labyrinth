{-# LANGUAGE TemplateHaskell #-}
module Labyrinth.Game
    ( Game (..)
    , currentPlayer
    , players
    , tiles
    , gates
    , playerByColor
    , fromPlayer
    , fromCurrentPlayer
    , nextPlayer
    , initialGame
    ) where

import           Data.Monoid         ((<>))
import           Control.Applicative ((<|>))
import           Lens.Micro          ((^.), (&), (%~), (.~))
import           Lens.Micro.TH       (makeLenses)
import qualified Labyrinth           as Labyrinth
import qualified Labyrinth.Players   as Players
import           Labyrinth.Players   (Player(..), Color(..), Players(..))
import qualified Labyrinth.Board     as Board
import           Labyrinth.Board     (Board, Position)
import           Labyrinth.Direction (Direction(..))
import qualified Labyrinth.Tile      as Tile
import           Labyrinth.Tile      (Tile(..), Terrain(..), goal)
import           Labyrinth.Gate      (Gate(..))
import qualified Labyrinth.Goal      as Goal
import           Labyrinth.Goal      (Goal)

data Game = Game
    { _currentPlayer       :: Maybe Player
    , _currentCellPosition :: Maybe Position
    , _players             :: Players
    , _tiles               :: Board Tile
    , _gates               :: Board Gate
    } deriving (Show, Eq)
makeLenses ''Game

instance Monoid Game where
  mempty = Game
    { _currentPlayer       = Nothing
    , _currentCellPosition = Nothing
    , _players             = mempty
    , _tiles               = mempty
    , _gates               = mempty
    }
  l `mappend` r = Game
    { _currentPlayer       = (r ^. currentPlayer) <|> (l ^. currentPlayer)
    , _currentCellPosition = (r ^. currentCellPosition) <|> (l ^. currentCellPosition)
    , _players             = (l ^. players) <> (r ^. players)
    , _tiles               = (r ^. tiles) <> (l ^. tiles)
    , _gates               = (r ^. gates) <> (l ^. gates)
    }

--------------------------------------------------------------------------------
-- games
--------------------------------------------------------------------------------

initialGame :: IO Game
initialGame = do
  ps <- Labyrinth.shuffle movingPositions
  mt <- Labyrinth.shuffle movingTiles >>= mapM Tile.randomRotate
  ts <- Labyrinth.shuffle Goal.treasures

  let fixedTiles = Board.fromList tileList
      movingTiles' = Board.fromList $ zip (defaultCellCurrentPosition:ps) mt
      (goals1, goals2, goals3) = distribute $ map Goal.fromTreasure ts

      tiles' = fromTiles $ mempty
        <> addGoals (Board.filterByPositions fixedGoalPositions fixedTiles) goals1
        <> addGoals (filterByTerrain Corner movingTiles') goals2
        <> addGoals (filterByTerrain Fork movingTiles') goals3
        <> fixedTiles
        <> movingTiles'

      gates' = fromGates $ Board.fromList gateList
      currentCellPosition' = fromCurrentCellPosition defaultCellCurrentPosition

  return $ gates' <> currentCellPosition' <> tiles'

addGoals :: Board Tile -> [Goal] -> Board Tile
addGoals b gs = Board.fromList $ map addGoal $ zip (Board.toList b) gs
  where
    addGoal :: ((Position, Tile), Goal) -> (Position, Tile)
    addGoal ((p, t), g) = (p, t & goal .~ Just g)

distribute :: [a] -> ([a], [a], [a])
distribute gs = (x, y, z)
  where
    (x, x') = Labyrinth.halve gs
    (y, z)  = Labyrinth.halve x'

filterByTerrain :: Terrain -> Board Tile -> Board Tile
filterByTerrain t = Board.filter byTerrain
  where
    byTerrain :: Tile -> Bool
    byTerrain (Tile t' _ _) = t == t'

fromGates :: Board Gate -> Game
fromGates g = mempty & gates .~ g

fromTiles :: Board Tile -> Game
fromTiles t = mempty & tiles .~ t

fromPlayer :: Player -> Game
fromPlayer p = mempty & players %~ (<> Players.fromPlayer p)

fromCurrentPlayer :: Player -> Game
fromCurrentPlayer p = mempty & currentPlayer .~ (Just p)

fromCurrentCellPosition :: Position -> Game
fromCurrentCellPosition p = mempty & currentCellPosition .~ (Just p)

--------------------------------------------------------------------------------
-- players
--------------------------------------------------------------------------------

playerByColor :: Color -> Game -> Maybe Player
playerByColor c g = Players.lookupByColor c (g ^. players)

nextPlayer :: Game -> Maybe Game
nextPlayer g = do
  currP <- g ^. currentPlayer
  nextP <- Players.next currP (g ^. players)
  return $ g & currentPlayer .~ (Just nextP)

--------------------------------------------------------------------------------
-- boards
--------------------------------------------------------------------------------

defaultCellCurrentPosition :: Position
defaultCellCurrentPosition = (2,0)

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
tileList = [ ((1, 1), Tile Corner South Nothing)
             , ((7, 1), Tile Corner West Nothing)
             , ((1, 7), Tile Corner East Nothing)
             , ((7, 7), Tile Corner North Nothing)
             , ((3, 1), Tile Fork South Nothing)
             , ((5, 1), Tile Fork South Nothing)
             , ((1, 3), Tile Fork East Nothing)
             , ((1, 5), Tile Fork East Nothing)
             , ((7, 3), Tile Fork West Nothing)
             , ((7, 5), Tile Fork West Nothing)
             , ((3, 7), Tile Fork North Nothing)
             , ((5, 7), Tile Fork North Nothing)
             , ((3, 3), Tile Fork East Nothing)
             , ((5, 3), Tile Fork South Nothing)
             , ((3, 5), Tile Fork North Nothing)
             , ((5, 5), Tile Fork West Nothing)
             ]

movingTiles :: [Tile]
movingTiles = replicate 12 (Tile Path North Nothing) ++
              replicate 16 (Tile Corner North Nothing) ++
              replicate 6  (Tile Fork North Nothing)

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
