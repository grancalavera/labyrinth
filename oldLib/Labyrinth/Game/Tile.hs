module Labyrinth.Game.Tile
  ( Tile(..)
  , Terrain(..)
  , Tokens
  , randomRotate
  , direction
  , treasure
  , terrain
  , rotate
  , rotate'
  , tokens
  , connected
  , tokenList
  )
where

import           Data.Set                       ( Set )
import qualified Data.Set                      as Set
import           Labyrinth.Game.Direction       ( Direction(..) )
import qualified Labyrinth.Game.Direction      as Direction
import           Labyrinth.Game.Treasure        ( Treasure )
import           Labyrinth.Game.Players         ( Color )
import           Lens.Micro                     ( (%~)
                                                , (^.)
                                                )
import           Lens.Micro.TH                  ( makeLenses )
import           System.Random                  ( randomRIO )

type Tokens = Set Color
data Terrain = Path | Corner | Fork deriving (Show, Eq)
data Tile = Tile
  { _terrain   :: Terrain
  , _direction :: Direction
  , _treasure  :: Maybe Treasure
  , _tokens    :: Tokens
  } deriving (Eq, Show)
makeLenses ''Tile

edges :: Tile -> Set Direction
edges t = Set.fromList $ case (t ^. terrain, t ^. direction) of
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

rotate :: Tile -> Tile
rotate = direction %~ Direction.previous

rotate' :: Tile -> Tile
rotate' = direction %~ Direction.next

randomRotate :: Tile -> IO Tile
randomRotate t = do
  i <- randomRIO (0, 3)
  let r = foldl (.) id $ replicate i rotate
  return (r t)

hasExit :: Direction -> Tile -> Bool
hasExit d = Set.member d . edges

connected :: Direction -> Tile -> Tile -> Bool
connected d exit enter = canExit && canEnter
 where
  canExit  = hasExit d exit
  canEnter = hasExit (Direction.opposite d) enter

tokenList :: Tile -> [Color]
tokenList = Set.toList . (^. tokens)
