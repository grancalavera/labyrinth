module Labyrinth.Game.Cell
  ( Cell(..)
  , TileCell(..)
  , GateCell(..)
  , CellData(..)
  , treasure
  , players
  , isOpen
  , terrain
  , direction
  , cellData
  , rotate
  , rotate'
  , randomRotate
  , hasExit
  , connected
  )
where

import qualified Data.Set                      as Set
import           Data.Set                       ( Set )
import           System.Random                  ( randomRIO )
import           Lens.Micro.TH                  ( makeLenses )
import           Lens.Micro                     ( (^.)
                                                , (%~)
                                                )
import qualified Labyrinth.Game.Direction      as Dir
import           Labyrinth.Game.Direction       ( Direction(..) )
import           Labyrinth.Game.Treasure        ( Treasure )
import           Labyrinth.Game.Player          ( Player )

data TileCell = TileCell
  { _treasure :: Maybe Treasure
  , _players :: Set Player
  } deriving (Show)

data GateCell = GateCell
  { _isOpen :: Bool
  } deriving (Show)

data Terrain = Gate | Path | Corner | Fork deriving (Show, Eq)
data CellData = TileData TileCell | GateData GateCell deriving (Show)

data Cell = Cell
  { _terrain   :: Terrain
  , _direction :: Direction
  , _cellData  :: CellData
  }

type Exits = Set Direction

makeLenses ''TileCell
makeLenses ''GateCell
makeLenses ''Cell

exits :: Cell -> Exits
exits t = Set.fromList $ case (t ^. terrain, t ^. direction) of

  (Gate  , North) -> [North]
  (Gate  , West ) -> [West]
  (Gate  , South) -> [South]
  (Gate  , East ) -> [East]

  (Path  , North) -> [North, South]
  (Path  , West ) -> [West, East]
  (Path  , South) -> [North, South]
  (Path  , East ) -> [West, East]

  (Corner, North) -> [North, West]
  (Corner, West ) -> [West, South]
  (Corner, South) -> [South, East]
  (Corner, East ) -> [East, North]

  (Fork  , North) -> [North, West, East]
  (Fork  , West ) -> [North, West, South]
  (Fork  , South) -> [West, South, East]
  (Fork  , East ) -> [South, East, North]

rotate :: Cell -> Cell
rotate = direction %~ Dir.previous

rotate' :: Cell -> Cell
rotate' = direction %~ Dir.next

randomRotate :: Cell -> IO Cell
randomRotate t = do
  i <- randomRIO (0, 3)
  let r = foldl (.) id $ replicate i rotate
  return (r t)

hasExit :: Direction -> Cell -> Bool
hasExit d = Set.member d . exits

connected :: Direction -> Cell -> Cell -> Bool
connected d exit enter = canExit && canEnter
 where
  canExit  = hasExit d exit
  canEnter = hasExit (Dir.opposite d) enter
