{-# LANGUAGE TemplateHaskell #-}

module Labyrinth.Tile
    ( Tile (..)
    , Terrain (..)
    , randomRotate
    , direction
    , terrain
    , goal
    , rotate
    , rotate'
    , edges
    ) where

import qualified Data.Set              as Set
import           Data.Set              (Set)
import           Lens.Micro.TH         (makeLenses)
import           Lens.Micro            ((%~))
import           System.Random         (randomRIO)
import           Labyrinth.Direction   (Direction(..))
import qualified Labyrinth.Direction   as Direction
import           Labyrinth.Goal        (Goal(..))

data Terrain = Path | Corner | Fork deriving (Show, Eq)
data Tile = Tile
  { _terrain :: Terrain
  , _direction :: Direction
  , _goal :: Maybe Goal
  } deriving (Eq, Show)
makeLenses ''Tile

edges :: Tile -> Set Direction
edges (Tile t d _) = Set.fromList $ case (t, d) of
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

rotate :: Tile -> Tile
rotate = direction %~ Direction.next

rotate' :: Tile -> Tile
rotate' = direction %~ Direction.previous

randomRotate :: Tile -> IO Tile
randomRotate t = do
  i <- randomRIO (0, 3)
  let r = foldl (.) id $ replicate i rotate
  return (r t)
