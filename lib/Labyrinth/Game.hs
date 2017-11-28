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
    ) where

import           Data.Monoid          ((<>))
import           Control.Applicative  ((<|>))
import           Lens.Micro           ((^.), (&), (%~), (.~))
import           Lens.Micro.TH        (makeLenses)
import           Labyrinth.Players    (Player(..), Color(..), Players(..))
import qualified Labyrinth.Players    as Players
import           Labyrinth.Board      (Board)
import           Labyrinth.Tile       (Tile(..))

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
