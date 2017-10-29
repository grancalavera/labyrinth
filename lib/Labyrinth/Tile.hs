{-# LANGUAGE TemplateHaskell #-}
module Labyrinth.Tile
    ( Edge (..)
    , Edges
    , Tile
    , Terrain (..)
    , edges
    , terrain
    , rotate
    , mirror
    , make
    , fromTerrain
    ) where

import qualified Data.Set as Set
import Data.Set           (Set)
import Lens.Micro         ((%~))
import Lens.Micro.TH      (makeLenses)

data Edge = North
          | West
          | South
          | East
          deriving (Show, Eq, Ord)

type Edges = Set Edge

data Terrain = Blank
             | Gate
             | Path
             | Corner
             | Fork
             deriving (Show, Eq)

data Tile = Tile
  { _terrain  :: Terrain
  , _edges    :: Edges
  } deriving (Show, Eq)
makeLenses ''Tile

fromTerrain :: Terrain -> Tile
fromTerrain t = make t (defaultEdges t)

make :: Terrain -> [Edge] -> Tile
make t es = Tile
  { _terrain = t
  , _edges = Set.fromList es
  }

rotate :: Tile -> Tile
rotate = edges %~ (Set.map nextEdge)

mirror :: Tile -> Tile
mirror = edges %~ (Set.map oppositeEdge)

nextEdge :: Edge -> Edge
nextEdge North  = West
nextEdge West   = South
nextEdge South  = East
nextEdge East   = North

oppositeEdge :: Edge -> Edge
oppositeEdge North  = South
oppositeEdge West   = East
oppositeEdge South  = North
oppositeEdge East   = West

defaultEdges :: Terrain -> [Edge]
defaultEdges Blank  = []
defaultEdges Gate   = [North]
defaultEdges Path   = [North, South]
defaultEdges Corner = [North, West]
defaultEdges Fork   = [North, West, East]
