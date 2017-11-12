{-# LANGUAGE TemplateHaskell #-}
module Labyrinth.Board
    ( Board
    , cells
    , fromCell
    ) where

import qualified Data.Map         as Map
import           Data.Map         (Map)
import           Lens.Micro.TH    (makeLenses)
import           Lens.Micro       ((^.))
import           Labyrinth.Tile   (Tile)

type Position = (Int, Int)
type Cell = (Position, Tile)
type Cells = Map Position Tile

data Board = Board
  { _cells:: Cells
  } deriving (Show)
makeLenses ''Board

instance Monoid Board where
  mempty = Board mempty
  l `mappend` r = Board (Map.union (r ^. cells) (l ^. cells))

fromCell :: Cell -> Board
fromCell (p, t) = Board (Map.insert p t mempty)
