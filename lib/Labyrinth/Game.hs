{-# LANGUAGE TemplateHaskell #-}
module Labyrinth.Game
    ( Game (..)
    , currentPlayer
    , players
    , board
    , gates
    , fromBoard
    , playerByColor
    , fromPlayer
    , fromCurrentPlayer
    , nextPlayer
    , initialGame
    , blankBoard
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
import           Labyrinth.Tile      (Tile(..), Terrain(..))
import qualified Labyrinth.Cell      as Cell
import           Labyrinth.Cell      (Cell)

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
    , _board               = mempty
    , _gates               = mempty
    , _openGates           = mempty
    }
  l `mappend` r = Game
    { _currentPlayer       = (r ^. currentPlayer) <|> (l ^. currentPlayer)
    , _currentCellPosition = (r ^. currentCellPosition) <|> (l ^. currentCellPosition)
    , _players             = (l ^. players) <> (r ^. players)
    , _board               = (l ^. board) <> (r ^. board)
    , _gates               = (l ^. gates) <> (r ^. gates)
    , _openGates           = (l ^. openGates) <> (r ^. openGates)
    }

--------------------------------------------------------------------------------
-- games
--------------------------------------------------------------------------------

initialGame :: IO Game
initialGame = do
  ps <- Labyrinth.shuffle movablePositions
  ts <- Labyrinth.shuffle movableTiles >>= mapM Labyrinth.rotateRandom

  let movable = zip (defaultCellCurrentPosition:ps) ts
      board'  = Board.fromList (map tileToCell (fixedTiles ++ movable))
      gates'  = Board.fromList (map tileToCell gateTiles)

  return $ fromBoard board' <>
           fromGates gates' <>
           fromCurrentCellPosition defaultCellCurrentPosition

fromGates :: Board -> Game
fromGates g = mempty & gates .~ g

fromBoard :: Board -> Game
fromBoard b = mempty & board .~ b

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

blankBoard :: Board
blankBoard = Board.fromList [((x, y), mempty) | x <- wRange, y <- hRange]
  where
    wRange :: [Int]
    wRange = [0..8]
    hRange :: [Int]
    hRange = [0..8]

defaultCellCurrentPosition :: Position
defaultCellCurrentPosition = (2,0)

tileToCell :: (Position, Tile) -> (Position, Cell)
tileToCell (p, t) = (p, Cell.fromTile t)

gateTiles :: [(Position, Tile)]
gateTiles = [ ((2, 0), Tile Gate South)
            , ((4, 0), Tile Gate South)
            , ((6, 0), Tile Gate South)
            , ((0, 2), Tile Gate East)
            , ((0, 4), Tile Gate East)
            , ((0, 6), Tile Gate East)
            , ((8, 2), Tile Gate West)
            , ((8, 4), Tile Gate West)
            , ((8, 6), Tile Gate West)
            , ((2, 8), Tile Gate North)
            , ((4, 8), Tile Gate North)
            , ((6, 8), Tile Gate North)
            ]

fixedTiles :: [(Position, Tile)]
fixedTiles = [ ((1, 1), Tile Corner South)
             , ((7, 1), Tile Corner West)
             , ((1, 7), Tile Corner East)
             , ((7, 7), Tile Corner North)
             , ((3, 1), Tile Fork South)
             , ((5, 1), Tile Fork South)
             , ((1, 3), Tile Fork East)
             , ((1, 5), Tile Fork East)
             , ((7, 3), Tile Fork West)
             , ((7, 5), Tile Fork West)
             , ((3, 7), Tile Fork North)
             , ((5, 7), Tile Fork North)
             , ((3, 3), Tile Fork East)
             , ((5, 3), Tile Fork South)
             , ((3, 5), Tile Fork North)
             , ((5, 5), Tile Fork West)
             ]

movableTiles :: [Tile]
movableTiles =
  replicate 12 (Tile Path North) ++
  replicate 16 (Tile Corner North) ++
  replicate 6  (Tile Fork North)

movablePositions :: [Position]
movablePositions =
  [(x,y) | x <- [2,4,6], y <- [1,3,5,7]] ++
  [(x,y) | x <- [1..7], y <- [2, 4, 6]]
