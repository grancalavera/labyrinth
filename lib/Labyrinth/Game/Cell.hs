module Labyrinth.Game.Cell
  ( Cell(..)
  , TileCell(..)
  , GateState(..)
  , Terrain(..)
  , treasure
  , players
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
import           Data.Set                                 ( Set )
import           Lens.Micro.TH                            ( makeLenses )
import           Lens.Micro                               ( (^.)
                                                          , (%~)
                                                          )
import           Control.Monad.Random.Strict              ( RandomGen
                                                          , Rand
                                                          , getRandomR
                                                          )
import qualified Labyrinth.Game.Direction      as Dir
import           Labyrinth.Game.Direction                 ( Direction(..) )
import           Labyrinth.Game.Treasure                  ( Treasure )
import           Labyrinth.Game.Player                    ( Player )

data TileCell = TileCell
  { _treasure :: Maybe Treasure
  , _players :: Set Player
  } deriving (Show, Eq)
makeLenses ''TileCell

data GateState = Closed | Open deriving(Show, Eq, Ord)

data Terrain = Gate | Path | Corner | Fork deriving (Show, Eq, Ord)

data Cell a = Cell
  { _terrain   :: Terrain
  , _direction :: Direction
  , _cellData  :: a
  } deriving (Show, Eq, Ord, Functor)
makeLenses ''Cell

type Exits = Set Direction

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

randomRotate :: RandomGen g => Cell a -> Rand g (Cell a)
randomRotate t = do
  i <- getRandomR (0, 3)
  let r = foldl (.) id $ replicate i rotate
  return (r t)

hasExit :: Direction -> Cell a -> Bool
hasExit d = Set.member d . exits

connected :: Direction -> Cell a -> Cell a -> Bool
connected d exit enter = canExit && canEnter
 where
  canExit  = hasExit d exit
  canEnter = hasExit (Dir.opposite d) enter
