module Labyrinth.Tile
    ( Tile(..)
    , Direction(..)
    , Terrain(..)
    , rotate
    , rotate'
    ) where

data Direction =  North
                | West
                | South
                | East
                deriving (Show, Eq, Ord)

data Terrain = Gate
             | Path
             | Corner
             | Fork
             deriving (Show, Eq)

data Tile = Tile Terrain Direction deriving (Show, Eq)

rotate :: Tile -> Tile
rotate (Tile t d) = Tile t (nextDirection d)

mirror :: Tile -> Tile
mirror (Tile t d) = Tile t (oppositeDirection d)

rotate' :: Tile -> Tile
rotate' = mirror . rotate

nextDirection :: Direction -> Direction
nextDirection North  = West
nextDirection West   = South
nextDirection South  = East
nextDirection East   = North

oppositeDirection :: Direction -> Direction
oppositeDirection = nextDirection . nextDirection
