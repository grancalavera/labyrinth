module Labyrinth.Game
  ( Player(..)
  , Players
  , Color(..)
  , PlayOrder(..)
  , Game(..)
  , Configuration
  , Position
  , defaultGame
  , playing
  )
where

import           Data.Set                     (Set)
import qualified Data.Set                     as Set
import           Linear.V2                    (V2 (..))

import           Labyrinth.Game.Board         (Board (..))
import qualified Labyrinth.Game.Board         as Board
import           Labyrinth.Game.Cell          (GateCell (..), Terrain (..))
import qualified Labyrinth.Game.Cell          as Cell
import           Labyrinth.Game.Class         (Game (..), playing)
import           Labyrinth.Game.Configuration (Configuration)
import           Labyrinth.Game.Direction     (Direction (..))
import           Labyrinth.Game.NewGame       (HasTreasure, TileD, newGame)
import           Labyrinth.Game.Player        (Color (..), PlayOrder (..),
                                               Player (..), Players)
import           Labyrinth.Game.Position      (Position)
import qualified Labyrinth.Game.Treasure      as T

defaultGame :: Players -> IO (Maybe Game)
defaultGame players =
  newGame tiles gates players 9 9 T.treasures extraTilePosition positions

gates :: Board GateCell
gates = Board.fromList $ map
  (\(p, d) -> (p, Cell.mkCell Gate d (GateCell True)))
  [ (V2 0 2, South)
  , (V2 0 4, South)
  , (V2 0 6, South)
  , (V2 2 0, East)
  , (V2 4 0, East)
  , (V2 6 0, East)
  , (V2 2 8, West)
  , (V2 4 8, West)
  , (V2 6 8, West)
  , (V2 8 2, North)
  , (V2 8 4, North)
  , (V2 8 6, North)
  ]

tiles :: [TileD HasTreasure PlayOrder]
tiles =
  [ (Corner, Just (V2 1 1), Just South, False, Just First)
    , (Corner, Just (V2 1 7), Just West , False, Just Second)
    , (Corner, Just (V2 7 1), Just East , False, Just Third)
    , (Corner, Just (V2 7 7), Just North, False, Just Fourth)
    , (Fork  , Just (V2 1 3), Just South, True , Nothing)
    , (Fork  , Just (V2 1 5), Just South, True , Nothing)
    , (Fork  , Just (V2 3 1), Just East , True , Nothing)
    , (Fork  , Just (V2 5 1), Just East , True , Nothing)
    , (Fork  , Just (V2 3 7), Just West , True , Nothing)
    , (Fork  , Just (V2 5 7), Just West , True , Nothing)
    , (Fork  , Just (V2 7 3), Just North, True , Nothing)
    , (Fork  , Just (V2 7 5), Just North, True , Nothing)
    , (Fork  , Just (V2 3 3), Just East , True , Nothing)
    , (Fork  , Just (V2 5 3), Just North, True , Nothing)
    , (Fork  , Just (V2 3 5), Just South, True , Nothing)
    , (Fork  , Just (V2 5 5), Just West , True , Nothing)
    ]
    <> replicate 12 (Path  , Nothing, Nothing, False, Nothing)
    <> replicate 6  (Corner, Nothing, Nothing, True , Nothing)
    <> replicate 10 (Corner, Nothing, Nothing, False, Nothing)
    <> replicate 6  (Fork  , Nothing, Nothing, True , Nothing)

positions :: Set Position
positions =
  Set.fromList $ extraTilePosition : [ V2 x y | x <- [1 .. 7], y <- [1 .. 7] ]

extraTilePosition :: Position
extraTilePosition = V2 0 2
