{-# LANGUAGE TemplateHaskell #-}
module Labyrinth.Game
    ( Game (..)
    , currentPlayer
    , players
    , tiles
    , gates
    , fromTiles
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
import           Labyrinth.Tile      (Tile(..), Terrain(..))
import           Labyrinth.Gate      (Gate(..))
import qualified Labyrinth.Cell      as Cell
import           Labyrinth.Cell      (Cell(..))

import           Data.Map            (Map)
import qualified Data.Map            as Map

data Game = Game
    { _currentPlayer       :: Maybe Player
    , _currentCellPosition :: Maybe Position
    , _players             :: Players
    , _tiles               :: Board Tile
    , _gates               :: Map Position Gate
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
    , _tiles               = (l ^. tiles) <> (r ^. tiles)
    , _gates               = (l ^. gates) <> (r ^. gates)
    }

--------------------------------------------------------------------------------
-- games
--------------------------------------------------------------------------------

initialGame :: IO Game
initialGame = do
  ps <- Labyrinth.shuffle movingPositions
  ts <- Labyrinth.shuffle movingCells >>= mapM Cell.randomRotate

  let movingCells' = zip (defaultCellCurrentPosition:ps) ts
      tiles'       = Board.fromList (fixedCells ++ movingCells')
      gates'       = Map.fromList gateCells

  return $ fromTiles tiles' <>
           fromGates gates' <>
           fromCurrentCellPosition defaultCellCurrentPosition

fromGates :: Map Position Gate -> Game
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

gateCells :: [(Position, Gate)]
gateCells = [ ((2, 0), Open)
            , ((4, 0), Open)
            , ((6, 0), Open)
            , ((0, 2), Open)
            , ((0, 4), Open)
            , ((0, 6), Open)
            , ((8, 2), Open)
            , ((8, 4), Open)
            , ((8, 6), Open)
            , ((2, 8), Open)
            , ((4, 8), Open)
            , ((6, 8), Open)
            ]

fixedCells :: [(Position, Cell Tile)]
fixedCells = [ ((1, 1), Cell South (Tile.fromTerrain Corner))
             , ((7, 1), Cell West (Tile.fromTerrain Corner))
             , ((1, 7), Cell East (Tile.fromTerrain Corner))
             , ((7, 7), Cell North (Tile.fromTerrain Corner))
             , ((3, 1), Cell South (Tile.fromTerrain Fork))
             , ((5, 1), Cell South (Tile.fromTerrain Fork))
             , ((1, 3), Cell East (Tile.fromTerrain Fork))
             , ((1, 5), Cell East (Tile.fromTerrain Fork))
             , ((7, 3), Cell West (Tile.fromTerrain Fork))
             , ((7, 5), Cell West (Tile.fromTerrain Fork))
             , ((3, 7), Cell North (Tile.fromTerrain Fork))
             , ((5, 7), Cell North (Tile.fromTerrain Fork))
             , ((3, 3), Cell East (Tile.fromTerrain Fork))
             , ((5, 3), Cell South (Tile.fromTerrain Fork))
             , ((3, 5), Cell North (Tile.fromTerrain Fork))
             , ((5, 5), Cell West (Tile.fromTerrain Fork))
             ]

movingCells :: [Cell Tile]
movingCells = replicate 12 (Cell North (Tile.fromTerrain Path)) ++
              replicate 16 (Cell North (Tile.fromTerrain Corner)) ++
              replicate 6  (Cell North (Tile.fromTerrain Fork))

movingPositions :: [Position]
movingPositions =
  [(x,y) | x <- [2,4,6], y <- [1,3,5,7]] ++
  [(x,y) | x <- [1..7], y <- [2, 4, 6]]
