module Labyrinth.Game.NewGame
  ( NewGame
  , newGame
  , resolveUnassignedPositions
  , resolveDirection
  )
where

import           Data.Set                       ( Set )
import qualified Data.Set                      as Set
import           Data.Maybe                     ( fromJust
                                                , isJust
                                                )

import           Labyrinth.Game.Cell            ( Terrain
                                                , GateCell
                                                )
import           Labyrinth.Game.Position        ( Position )
import           Labyrinth.Game.Treasure        ( Treasure )
import qualified Labyrinth.Game.Direction      as D
import           Labyrinth.Game.Direction       ( Direction )
import           Labyrinth.Game.Player          ( Players )
import           Labyrinth.Game.Class           ( Game )

type AddTreasure = Bool
type CellD = (Terrain, Maybe Position, Maybe Direction, AddTreasure, Players)
type ResolvedCellD = (Terrain, Position, Direction, Maybe Treasure, Players)
type GateD = (Position, GateCell)

data NewGame = NewGame
  { cells     :: [CellD]
  , gates     :: [GateD]
  , players   :: Players
  , rowCount  :: Int
  , colCount  :: Int
  , treasures :: [Treasure]
  , extraTile :: Position
  , positions :: Set Position
  } deriving (Show)

newGame
  :: [CellD]
  -> [GateD]
  -> Players
  -> Int
  -> Int
  -> [Treasure]
  -> Position
  -> Set Position
  -> IO (Maybe Game)
newGame cells gates players rowCount colCount treasures extraTile positions =
  undefined

resolveUnassignedPositions
  :: Set Position -> Set (Maybe Position) -> Set Position
resolveUnassignedPositions p mp =
  Set.map fromJust $ Set.difference (Set.map Just p) mp

resolveDirection :: Maybe Direction -> IO Direction
resolveDirection md | isJust md = return $ fromJust md
                    | otherwise = D.random
