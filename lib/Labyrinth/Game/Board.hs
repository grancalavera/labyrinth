module Labyrinth.Game.Board
  ( Board(..)
  , fromList
  )
where

import           Data.Map.Strict                          ( Map )
import qualified Data.Map.Strict               as Map
import           Labyrinth.Game.Position                  ( Position )
import           Labyrinth.Game.Cell                      ( Cell )

newtype Board a = Board { toMap :: Map Position (Cell a)} deriving (Show)

fromList :: [(Position, (Cell a))] -> Board a
fromList = Board . Map.fromList
