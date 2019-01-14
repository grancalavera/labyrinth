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
import           Labyrinth.Game.Cell            ( Cell )
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
  , _tiles     :: Map Position Cell
  , _gates     :: Map Position Cell
  , _treasures :: Map Player TreasureMap
  } deriving (Show)
makeLenses  ''Game
