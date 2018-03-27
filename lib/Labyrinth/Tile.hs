{-# LANGUAGE NamedFieldPuns  #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module Labyrinth.Tile
    ( Tile (..)
    , Terrain (..)
    , randomRotate
    , direction
    , goal
    , terrain
    , rotate
    , rotate'
    , edges
    , players
    ) where

import           Data.Set            (Set)
import qualified Data.Set            as Set
import           Labyrinth.Direction (Direction (..))
import qualified Labyrinth.Direction as Direction
import           Labyrinth.Goal      (Goal)
import           Labyrinth.Players   (Player)
import           Lens.Micro          ((%~), (^.))
import           Lens.Micro.TH       (makeLenses)
import           System.Random       (randomRIO)

data Terrain = Path | Corner | Fork deriving (Show, Eq)
data Tile = Tile
  { _terrain   :: Terrain
  , _direction :: Direction
  , _goal      :: Maybe Goal
  , _players   :: [Player]
  } deriving (Eq, Show)
makeLenses ''Tile

edges :: Tile -> Set Direction
edges t = Set.fromList $ case (t ^. terrain, t ^. direction) of
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
rotate = direction %~ Direction.previous

rotate' :: Tile -> Tile
rotate' = direction %~ Direction.next

randomRotate :: Tile -> IO Tile
randomRotate t = do
  i <- randomRIO (0, 3)
  let r = foldl (.) id $ replicate i rotate
  return (r t)
