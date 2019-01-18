module Labyrinth.Game.Cell
  ( Cell(..)
  , TileCell(..)
  , GateCell(..)
  , CellData(..)
  , Terrain
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

data Terrain = Gate | Path | Corner | Fork deriving (Show, Eq)
type Exits = Set Direction

data TileCell = TileCell
  { _treasure :: Maybe Treasure
  , _players :: Set Player
  } deriving (Show)
makeLenses ''TileCell

data GateCell = GateCell
  { _isOpen :: Bool
  } deriving (Show)
makeLenses ''GateCell

newtype CellData a = CellData a deriving (Show)

data Cell a = Cell
  { _terrain   :: Terrain
  , _direction :: Direction
  , _cellData  :: CellData a
  } deriving (Show)
makeLenses ''Cell

exits :: Cell a -> Exits
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

rotate :: Cell a -> Cell a
rotate = direction %~ Dir.previous

rotate' :: Cell a -> Cell a
rotate' = direction %~ Dir.next

randomRotate :: Cell a -> IO (Cell a)
randomRotate t = do
  i <- randomRIO (0, 3)
  let r = foldl (.) id $ replicate i rotate
  return (r t)

hasExit :: Direction -> Cell a -> Bool
hasExit d = Set.member d . exits

connected :: Direction -> Cell a -> Cell a -> Bool
connected d exit enter = canExit && canEnter
 where
  canExit  = hasExit d exit
  canEnter = hasExit (Dir.opposite d) enter
