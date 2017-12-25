module Labyrinth.Tile
    ( Tile(..)
    , Terrain(..)
    , exits
    , fromTerrain
    ) where

import qualified Data.Set              as Set
import           Labyrinth.Direction   (Direction(..))
import           Labyrinth.Transitable (Transitable, exits)
import           Labyrinth.Goal        (Goal(..))

data Terrain = Path | Corner | Fork deriving (Show, Eq)
data Tile = Tile Terrain (Maybe Goal) deriving (Show, Eq)

fromTerrain :: Terrain -> Tile
fromTerrain t = Tile t Nothing

instance Transitable Tile where
  exits (Tile t _) d = Set.fromList $ case (t, d) of
    (Path, North)   -> [North, South]
    (Path, West)    -> [West, East]
    (Path, South)   -> [North, South]
    (Path, East)    -> [West, East]
    (Corner, North) -> [North, West]
    (Corner, West)  -> [West, South]
    (Corner, South) -> [South, East]
    (Corner, East)  -> [East, North]
    (Fork, North)   -> [North, West, East]
    (Fork, West)    -> [North, West, South]
    (Fork, South)   -> [West, South, East]
    (Fork, East)    -> [South, East, North]
