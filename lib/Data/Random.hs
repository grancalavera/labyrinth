module Data.Random
  ( choose
  , shuffle
  )
where

import           Data.List.NonEmpty                       ( NonEmpty )
import qualified Data.List.NonEmpty            as NE
import           Control.Monad.Random.Strict              ( RandomGen
                                                          , Rand
                                                          , getRandomR
                                                          )

choose :: RandomGen g => NonEmpty a -> Rand g a
choose xs = (xs NE.!!) <$> getRandomR (0, length xs - 1)

shuffle :: (RandomGen g, Eq a) => [a] -> Rand g [a]
shuffle [] = return []
shuffle xs = do
  x'  <- choose $ NE.fromList xs
  xs' <- shuffle $ filter (x' /=) xs
  return (x' : xs')
