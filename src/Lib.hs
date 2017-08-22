module Lib where
import Data.List (intercalate)
import Control.Monad (liftM)

printBoard :: IO ()
printBoard = putStrLn $ show board

type Cell = String
data Board = Board [Cell] deriving Eq

instance Show Board where
  show board =  intercalate "\n" (liftM (intercalate " ") $ toGrid board)

board = Board (map toCell [1..(9*9)])

toCell :: Int -> Cell
toCell = \_ -> "[___]"

getBoard :: Board -> [Cell]
getBoard (Board board) = board

toGrid :: Board -> [[Cell]]
toGrid (Board board) = toGrid' board
  where
    toGrid' :: [Cell] -> [[Cell]]
    toGrid' b = split (splitAt 9 b)

    split :: ([Cell], [Cell]) -> [[Cell]]
    split ([], _) = []
    split (xs, ys) = xs : toGrid' ys
