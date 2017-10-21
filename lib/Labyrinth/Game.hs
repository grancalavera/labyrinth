{-# LANGUAGE TemplateHaskell #-}
module Labyrinth.Game
    ( Game (..)
    , currentPlayer
    , players
    , currentTile
    , board
    , playerByColor
    , fromPlayer
    , fromCurrentPlayer
    , fromBoard
    , fromCurrentTile
    , nextPlayer
    ) where

import           Data.Monoid          ((<>))
import           Control.Applicative  ((<|>))
import           Lens.Micro           ((^.), (&), (%~), (.~))
import           Lens.Micro.TH        (makeLenses)
import           Labyrinth.Players    (Player(..), Color(..), Players(..))
import qualified Labyrinth.Players    as Players

-- temp
data Tile   = Tile deriving (Show, Eq)
data Board  = BT Tile | Done deriving (Show, Eq)

data Game = Game
    { _currentPlayer :: Maybe Player
    , _players :: Players
    , _currentTile :: Maybe Tile
    , _board :: Board
    } deriving (Show, Eq)
makeLenses ''Game

instance Monoid Game where
  mempty = Game
    { _currentPlayer  = Nothing
    , _players        = mempty
    , _currentTile    = Nothing
    , _board          = Done
    }
  l `mappend` r = Game
    { _currentPlayer  = (r ^. currentPlayer) <|> (l ^. currentPlayer)
    , _players        = (l ^. players) <> (r ^. players)
    , _currentTile    = (r ^. currentTile) <|> (l ^. currentTile)
    , _board          = Done
    }

playerByColor :: Color -> Game -> Maybe Player
playerByColor c g = Players.lookupByColor c (g ^. players)

fromPlayer :: Player -> Game
fromPlayer p = mempty & players %~ (<> Players.fromPlayer p)

fromCurrentPlayer :: Player -> Game
fromCurrentPlayer p = mempty & currentPlayer .~ (Just p)

nextPlayer :: Game -> Maybe Game
nextPlayer g = do
  currP <- g ^. currentPlayer
  nextP <- Players.next currP (g ^. players)
  return $ g & currentPlayer .~ (Just nextP)

fromBoard :: Board -> Game
fromBoard = undefined

fromCurrentTile :: Tile -> Game
fromCurrentTile = undefined
