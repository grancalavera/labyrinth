module Labyrinth.Game.Board
  ( Board(..)
  )
where

import           Data.Map.Strict                ( Map )
import           Labyrinth.Game.Position        ( Position )
import           Labyrinth.Game.Cell            ( Cell )

newtype Board a = Board { toMap :: Map Position (Cell a)} deriving (Show)
