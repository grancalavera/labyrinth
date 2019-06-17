module Labyrinth.Game.Internal
  ( Game(..)
  , players
  , playing
  , extraTile
  , rowCount
  , colCount
  , tiles
  , gates
  , treasures
  )
where

import           Data.Map.Strict                                    ( Map )
import           Control.Lens                                       ( makeLenses )

import           Labyrinth.Game.Board                               ( Board )
import           Labyrinth.Game.Cell                                ( TileCell
                                                                    , GateState
                                                                    )
import           Labyrinth.Game.Player                              ( Player
                                                                    , Players
                                                                    )
import           Labyrinth.Game.Position                            ( Position )
import           Labyrinth.Game.Treasure                            ( TreasureMap )

data Game = Game
  { _players   :: Players
  , _playing   :: Player
  , _extraTile :: Position
  , _rowCount  :: Int
  , _colCount  :: Int
  , _tiles     :: Board TileCell
  , _gates     :: Board GateState
  , _treasures :: Map Player TreasureMap
  } deriving (Show)
makeLenses ''Game
