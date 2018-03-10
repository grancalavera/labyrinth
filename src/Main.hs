module Main where
import           Labyrinth.UI   (addPlayers, playGame)
import qualified Labyrinth.Game as Game
import           Labyrinth.Game (Game)
import           Lens.Micro

import Labyrinth (getRow, getCol, splitEvery, rowIndex, colIndex)
import Labyrinth.Direction (Direction(..))
import qualified Data.Map as Map
import qualified Data.List as L

main :: IO ()
main = do
  players <- addPlayers
  playGame players

f = addPlayers >>= Game.initialGame >>= \g -> do
  print "North"
  print $ Game.moves East $ g & Game.currentTilePosition .~ (0,4)
  print $ Game.moves West $ g & Game.currentTilePosition .~ (0,4)
  print $ Game.moves South $ g & Game.currentTilePosition .~ (0,4)

  print "South"
  print $ Game.moves East $ g & Game.currentTilePosition .~ (8,4)
  print $ Game.moves West $ g & Game.currentTilePosition .~ (8,4)
  print $ Game.moves North $ g & Game.currentTilePosition .~ (8,4)

  print "West"
  print $ Game.moves North $ g & Game.currentTilePosition .~ (4,0)
  print $ Game.moves South $ g & Game.currentTilePosition .~ (4,0)
  print $ Game.moves East $ g & Game.currentTilePosition .~ (4,0)

  print "East"
  print $ Game.moves North $ g & Game.currentTilePosition .~ (4,8)
  print $ Game.moves South $ g & Game.currentTilePosition .~ (4,8)
  print $ Game.moves West $ g & Game.currentTilePosition .~ (4,8)


