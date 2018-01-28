module Main where
import Labyrinth.UI (addPlayers, playGame)

main :: IO ()
main = do
  players <- addPlayers
  playGame players
