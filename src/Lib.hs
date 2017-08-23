module Lib where
import Data.List (intercalate)
import Control.Monad (liftM)

printBoard :: IO ()
printBoard = putStrLn $ show board

cols :: Int
cols = 9
size :: Int
size = cols - 1

board :: Board
board = Board (map toCell [0..(cols*cols)-1])

type Cell = String
data Board = Board [Cell] deriving Eq

instance Show Board where
  show board =  intercalate "\n" (liftM (intercalate " ") $ toGrid board)

toCell :: Int -> Cell
toCell i = (pad i) ++ "|" ++ cell
  where cell | isCorner i = "***"
             | isEdge i = " + "
             | otherwise = "   "

getBoard :: Board -> [Cell]
getBoard (Board board) = board

pad :: (Show a, Num a) => a -> String
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

coords :: Int -> Int -> (Int, Int)
coords cols index = (x, y)
  where x = index `rem` cols
        y = index `div` cols

getCoords = coords cols

mapPoint :: (Int -> b) -> (Int, Int) -> (b, b)
mapPoint f (x, y) = (f x, f y)

isEdge :: Int -> Bool
isEdge i = isEdge' (getCoords i)
  where isEdge' (x,y) = (x==0) || (y==0) || (x==size) || (y==size)

isCorner :: Int -> Bool
isCorner i = isCorner' (getCoords i)
  where isCorner' (x,y) | (x==0)    && (y==0)    = True
                        | (x==size) && (y==0)    = True
                        | (x==size) && (y==size) = True
                        | (x==0)    && (y==size) = True
                        | otherwise              = False
