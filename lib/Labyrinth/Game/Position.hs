module Labyrinth.Game.Position
  ( Position
  , Rows
  , Cols
  )
where
import           Linear.V2                      ( V2 )

type Rows = Int
type Cols = Int
type Position = V2 Int

-- the total width of the board minus 2
-- to add padding around the boart to
-- render the gates
-- boardPositions :: Position -> Rows -> Cols -> [Position]
-- boardPositions extraTile rows cols = undefined
