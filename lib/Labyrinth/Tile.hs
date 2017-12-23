module Labyrinth.Tile
    ( Tile(..)
    , Terrain(..)
    ) where

import qualified Data.Set              as Set
import           Labyrinth.Direction   (Direction(..))
import           Labyrinth.Transitable (Transitable, exits)
import           Labyrinth.Goal        (Goal(..))

data Terrain = Path | Corner | Fork deriving (Show, Eq)
data Tile = Tile Terrain (Maybe Goal) deriving (Show, Eq)

instance Transitable Tile where
  exits d (Tile t _) = Set.fromList $ case (t, d) of
    (Path, North)   -> [North, South]
    (Path, South)   -> [North, South]
    (Path, West)    -> [West, East]
    (Path, East)    -> [West, East]
    (Corner, North) -> [North, West]
    (Corner, West)  -> [West, South]
    (Corner, South) -> [South, East]
    (Corner, East)  -> [East, North]
    (Fork, North)   -> [North, West, East]
    (Fork, West)    -> [North, West, South]
    (Fork, South)   -> [West, South, East]
    (Fork, East)    -> [South, East, North]

  -- Tile Gate North   -> [North]
  -- Tile Gate South   -> [South]
  -- Tile Gate West    -> [West]
  -- Tile Gate East    -> [East]
