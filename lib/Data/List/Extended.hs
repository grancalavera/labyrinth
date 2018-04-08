module Data.List.Extended
  ( module Data.List
  , splitEvery
  , middle
  , safeLast
  , safeHead
  )
where

import           Data.List

splitEvery :: Int -> [a] -> [[a]]
splitEvery _ []   = []
splitEvery n list = first : splitEvery n rest
  where (first, rest) = splitAt n list

middle :: [a] -> Maybe [a]
middle xs = safe (drop 1) xs >>= safe init

safeLast :: [a] -> Maybe a
safeLast = safe last

safeHead :: [a] -> Maybe a
safeHead = safe head

safe :: ([a] -> b) -> [a] -> Maybe b
safe _ [] = Nothing
safe f xs = Just $ f xs
