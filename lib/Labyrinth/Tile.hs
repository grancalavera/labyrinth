module Labyrinth.Tile
    ( Tile(..)
    , Direction(..)
    , Terrain(..)
    , rotate
    , rotate'
    , edges
    ) where

import           Data.Set (Set)
import qualified Data.Set as Set

data Direction =  North
                | West
                | South
                | East
                deriving (Show, Eq, Ord)

data Terrain = Blank
             | Gate
             | Path
             | Corner
             | Fork
             deriving (Show, Eq)

data Tile = Tile Terrain Direction deriving (Show, Eq)

rotate :: Tile -> Tile
rotate (Tile t d) = Tile t (nextDirection d)

rotate' :: Tile -> Tile
rotate' = mirror . rotate

edges :: Tile -> Set Direction
edges t = Set.fromList $ case t of
  Tile Gate North   -> [North]
  Tile Gate South   -> [South]
  Tile Gate West    -> [West]
  Tile Gate East    -> [East]
  Tile Corner North -> [North, West]
  Tile Corner West  -> [West, South]
  Tile Corner East  -> [East, North]
  Tile Corner South -> [South, East]
  Tile Fork East    -> [North, South, East]
  Tile Fork West    -> [North, West, South]
  Tile Fork South   -> [West, South, East]
  Tile Path North   -> vpath
  Tile Path South   -> vpath
  Tile Path West    -> hpath
  Tile Path East    -> hpath
  where
    vpath = [North, South]
    hpath = [West, East]

mirror :: Tile -> Tile
mirror (Tile t d) = Tile t $ (nextDirection . nextDirection) d

nextDirection :: Direction -> Direction
nextDirection North  = West
nextDirection West   = South
nextDirection South  = East
nextDirection East   = North
