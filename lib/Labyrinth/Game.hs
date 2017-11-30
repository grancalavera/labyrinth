{-# LANGUAGE TemplateHaskell #-}
module Labyrinth.Game
    ( Game (..)
    , currentPlayer
    , players
    , board
    , fromBoard
    , playerByColor
    , fromPlayer
    , fromCurrentPlayer
    , nextPlayer
    , initialGame
    ) where

import           Data.Monoid          ((<>))
import           Control.Applicative  ((<|>))
import           Lens.Micro           ((^.), (&), (%~), (.~))
import           Lens.Micro.TH        (makeLenses)
-- import qualified Data.Array.IO        as ArrayIO
import           System.Random        (randomRIO)
import           Labyrinth.Players    (Player(..), Color(..), Players(..))
import qualified Labyrinth.Players    as Players
import           Labyrinth.Board      (Board, Cell(..), Position)
import qualified Labyrinth.Board      as Board
import           Labyrinth.Tile       (Tile(..), Terrain(..), Direction(..))

data Game = Game
    { _currentPlayer  :: Maybe Player
    , _players        :: Players
    , _board          :: Board Tile
    } deriving (Show, Eq)
makeLenses ''Game

instance Monoid Game where
  mempty = Game
    { _currentPlayer  = Nothing
    , _players        = mempty
    , _board          = mempty
    }
  l `mappend` r = Game
    { _currentPlayer  = (r ^. currentPlayer) <|> (l ^. currentPlayer)
    , _players        = (l ^. players) <> (r ^. players)
    , _board          = (l ^. board) <> (r ^. board)
    }

fromPlayer :: Player -> Game
fromPlayer p = mempty & players %~ (<> Players.fromPlayer p)

fromCurrentPlayer :: Player -> Game
fromCurrentPlayer p = mempty & currentPlayer .~ (Just p)

fromBoard :: Board Tile -> Game
fromBoard b = mempty & board .~ b

playerByColor :: Color -> Game -> Maybe Player
playerByColor c g = Players.lookupByColor c (g ^. players)

nextPlayer :: Game -> Maybe Game
nextPlayer g = do
  currP <- g ^. currentPlayer
  nextP <- Players.next currP (g ^. players)
  return $ g & currentPlayer .~ (Just nextP)

initialGame :: IO Game
initialGame = return (fromBoard fixedTiles)

fixedTiles :: Board Tile
fixedTiles = Board.fromCells
  [ Cell (2, 0) (Tile Gate South)
  , Cell (4, 0) (Tile Gate South)
  , Cell (6, 0) (Tile Gate South)
  , Cell (0, 2) (Tile Gate East)
  , Cell (0, 4) (Tile Gate East)
  , Cell (0, 6) (Tile Gate East)
  , Cell (8, 2) (Tile Gate West)
  , Cell (8, 4) (Tile Gate West)
  , Cell (8, 6) (Tile Gate West)
  , Cell (2, 8) (Tile Gate North)
  , Cell (4, 8) (Tile Gate North)
  , Cell (6, 8) (Tile Gate North)
  , Cell (1, 1) (Tile Corner South)
  , Cell (7, 1) (Tile Corner West)
  , Cell (1, 7) (Tile Corner East)
  , Cell (7, 7) (Tile Corner North)
  , Cell (3, 1) (Tile Fork South)
  , Cell (5, 1) (Tile Fork South)
  , Cell (1, 3) (Tile Fork East)
  , Cell (1, 5) (Tile Fork East)
  , Cell (7, 3) (Tile Fork West)
  , Cell (7, 5) (Tile Fork West)
  , Cell (3, 7) (Tile Fork North)
  , Cell (5, 7) (Tile Fork North)
  , Cell (3, 3) (Tile Fork East)
  , Cell (5, 3) (Tile Fork South)
  , Cell (3, 5) (Tile Fork North)
  , Cell (5, 5) (Tile Fork West)
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

-- shuffle :: [a] -> IO [a]
-- shuffle xs = do
--   ar <- newArray n xs
--   forM [1..n] $ \i -> do
--     j <- randomRIO (i, n)
--     vi <- ArrayIO.readArray ar i
--     vj <- ArrayIO.readArray ar j
--     ArrayIO.writeArray ar j vi
--     return vj
--   where
--     n = length xs
--     newArray :: Int -> [a] -> IO (IOArray Int a)
--     newArray n' xs' = ArrayIO.newListArray (1, n') xs'
