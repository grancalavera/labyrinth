module Lib where

printBoard :: IO ()
printBoard = putStrLn $ show board

type Cell = String
data Board = Board [Cell]

instance Show Board where
  show (Board board) = show board

board = Board (map toCell [1..(9*9)])

-- board = concat $ ([
--   (concatMap toCell [1..9]) ++ "\n",
--   (concatMap toCell [1..9]) ++ "\n",
--   (concatMap toCell [1..9]) ++ "\n",
--   (concatMap toCell [1..9]) ++ "\n",
--   (concatMap toCell [1..9]) ++ "\n",
--   (concatMap toCell [1..9]) ++ "\n",
--   (concatMap toCell [1..9]) ++ "\n",
--   (concatMap toCell [1..9]) ++ "\n",
--   (concatMap toCell [1..9]) ++ "\n"
--   ])

toCell :: Int -> Cell
toCell = \_ -> "[  ]"

getBoard :: Board -> [Cell]
getBoard (Board board) = board

toGrid :: [Cell] -> [[Cell]]
toGrid board = toGrid' (splitAt 9 board)
  where
    toGrid' ([],_) = []
    toGrid' (xs, ys) = xs : toGrid ys
