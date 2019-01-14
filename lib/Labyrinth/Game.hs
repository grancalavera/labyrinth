module Labyrinth.Game
  ( Player(..)
  , Players
  , Color(..)
  , PlayOrder(..)
  , Game(..)
  , Configuration
  , players
  , playing
  )
where

import           Lens.Micro.TH                  ( makeLenses )
import           Labyrinth.Game.Position        ( Position )
import           Labyrinth.Game.Board           ( Board )
import           Labyrinth.Game.Cell            ( TileCell
                                                , GateCell
                                                )
import           Labyrinth.Game.Treasure        ( TreasureMap )
import           Data.Map.Strict                ( Map )
import           Labyrinth.Game.Player          ( Player(..)
                                                , Players
                                                , Color(..)
                                                , PlayOrder(..)
                                                )
import           Labyrinth.Game.Configuration   ( Configuration )

data Game = Game
  { _players   :: Players
  , _playing   :: Player
  , _extraTile :: Position
  , _rowCount  :: Int
  , _colCount  :: Int
  , _tiles     :: Board TileCell
  , _gates     :: Board GateCell
  , _treasures :: Map Player TreasureMap
  } deriving (Show)
makeLenses  ''Game
