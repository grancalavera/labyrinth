module Lib where
import Data.List (intercalate)
import Control.Monad (liftM)

printBoard :: IO ()
printBoard = putStrLn $ show board

cols :: Integral a => a
cols = 9
type Cell = String
data Board = Board [Cell] deriving Eq

instance Show Board where
  show board =  intercalate "\n" (liftM (intercalate " ") $ toGrid board)

board = Board (map toCell [1..(cols*cols)])

toCell :: Int -> Cell
toCell = \i -> "[" ++ (pad i) ++ "]"

getBoard :: Board -> [Cell]
getBoard (Board board) = board

pad :: (Show a, Integral a) => a -> String
pad i = pad' (show i)
  where pad' s | length s > 2 = ""
               | length s == 2 = s
               | otherwise = pad' ("0"++s)

padPoint = mapPoint pad

toGrid :: Board -> [[Cell]]
toGrid (Board board) = toGrid' board
  where
    toGrid' :: [Cell] -> [[Cell]]
    toGrid' b = split (splitAt cols b)

    split :: ([Cell], [Cell]) -> [[Cell]]
    split ([], _) = []
    split (xs, ys) = xs : toGrid' ys

coords :: Integral a => a -> a -> (a, a)
coords cols index = (x, y)
  where x = index `mod` cols
        y = index `div` cols

getCoords = coords cols

mapPoint :: Integral a => (a -> b) -> (a, a) -> (b, b)
mapPoint f (x, y) = (f x, f y)
