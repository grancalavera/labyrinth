module Main
where


import           UI.Players                     ( addPlayers )
import           UI.Game                        ( playGame )

main :: IO ()
main = do
  p <- addPlayers
  playGame p


