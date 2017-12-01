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

    -- temp
    , movableTiles
    , movablePositions
    -- temp

    ) where

import           Data.Monoid          ((<>))
import           Control.Applicative  ((<|>))
import           Lens.Micro           ((^.), (&), (%~), (.~))
import           Lens.Micro.TH        (makeLenses)
-- import qualified Data.Array.IO        as ArrayIO
-- import           Control.Monad        (forM)
import           System.Random        (randomRIO)
import           Labyrinth.Players    (Player(..), Color(..), Players(..))
import qualified Labyrinth.Players    as Players
import           Labyrinth.Board      (Board, Position)
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
fixedTiles = Board.fromList
  [ ((2, 0), (Tile Gate South))
  , ((4, 0), (Tile Gate South))
  , ((6, 0), (Tile Gate South))
  , ((0, 2), (Tile Gate East))
  , ((0, 4), (Tile Gate East))
  , ((0, 6), (Tile Gate East))
  , ((8, 2), (Tile Gate West))
  , ((8, 4), (Tile Gate West))
  , ((8, 6), (Tile Gate West))
  , ((2, 8), (Tile Gate North))
  , ((4, 8), (Tile Gate North))
  , ((6, 8), (Tile Gate North))
  , ((1, 1), (Tile Corner South))
  , ((7, 1), (Tile Corner West))
  , ((1, 7), (Tile Corner East))
  , ((7, 7), (Tile Corner North))
  , ((3, 1), (Tile Fork South))
  , ((5, 1), (Tile Fork South))
  , ((1, 3), (Tile Fork East))
  , ((1, 5), (Tile Fork East))
  , ((7, 3), (Tile Fork West))
  , ((7, 5), (Tile Fork West))
  , ((3, 7), (Tile Fork North))
  , ((5, 7), (Tile Fork North))
  , ((3, 3), (Tile Fork East))
  , ((5, 3), (Tile Fork South))
  , ((3, 5), (Tile Fork North))
  , ((5, 5), (Tile Fork West))
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
