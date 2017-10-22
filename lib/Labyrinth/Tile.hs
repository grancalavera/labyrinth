{-# LANGUAGE TemplateHaskell #-}
module Labyrinth.Tile
    ( Edge (..)
    , Tile (..)
    , Terrain (..)
    , edges
    , terrain
    , rotate
    , mirror
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

data Terrain = Blank
             | Gate
             | Path
             | Corner
             | Fork
             deriving (Show, Eq)

data Tile = Tile
  { _terrain  :: Terrain
  , _edges    :: Set Edge
  } deriving (Show, Eq)
makeLenses ''Tile

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

-- makeTile :: Terrain -> Tile
-- makeTile terrain' = Tile terrain' (Set.fromList edges')
--   where
--     edges' = case terrain' of
--       Blank   -> []
--       Path    -> [North, South]
--       Corner  -> [North, West]
--       Fork    -> [North, West, East]

