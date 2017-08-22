module Lib where

printBoard :: IO ()
printBoard = putStrLn board


board :: String
board = concat $ ([
  (concatMap cell [1..9]) ++ "\n",
  (concatMap cell [1..9]) ++ "\n",
  (concatMap cell [1..9]) ++ "\n",
  (concatMap cell [1..9]) ++ "\n",
  (concatMap cell [1..9]) ++ "\n",
  (concatMap cell [1..9]) ++ "\n",
  (concatMap cell [1..9]) ++ "\n",
  (concatMap cell [1..9]) ++ "\n",
  (concatMap cell [1..9]) ++ "\n"
  ])

cell :: Int -> String
cell = \_ -> "[TOP] "

-- T: type
-- O: objective
-- P: player
