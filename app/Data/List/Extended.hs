module Data.List.Extended
  ( module Data.List
  , splitEvery
  )
where

import           Data.List

splitEvery :: Int -> [a] -> [[a]]
splitEvery _ []   = []
splitEvery n list = first : splitEvery n rest
  where (first, rest) = splitAt n list
