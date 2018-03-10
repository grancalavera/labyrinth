module Main where
import           Labyrinth.UI   (addPlayers, playGame)

main :: IO ()
main = addPlayers >>= playGame
