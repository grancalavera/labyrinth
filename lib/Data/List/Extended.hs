module Data.List.Extended
  ( module Data.List
  , splitEvery
  , middle
  , safeLast
  )
where

import           Data.List

splitEvery :: Int -> [a] -> [[a]]
splitEvery _ []   = []
splitEvery n list = first : splitEvery n rest
  where (first, rest) = splitAt n list

middle :: [a] -> Maybe [a]
middle xs = safe (drop 1) xs >>= safe init

safe :: ([a] -> [a]) -> [a] -> Maybe [a]
safe _ []  = Nothing
safe f xs  = Just $ f xs

safeLast :: [a] -> Maybe a
safeLast [] = Nothing
safeLast xs = Just $ last xs
