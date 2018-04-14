{-# LANGUAGE TemplateHaskell #-}

module Labyrinth.Players
  ( Color(..)
  , colors
  , Player(..)
  , name
  , Players
  , toMap
  , toList
  , fromList
  , next
  )
where

import           Data.Map.Strict                ( Map )
import qualified Data.Map.Strict               as Map
import           Data.Maybe                     ( fromJust )
import qualified Data.List                     as L
import qualified Data.Text                     as T
import           Lens.Micro.TH                  ( makeLenses )

data Color = Yellow | Red  | Blue | Green deriving (Show, Eq, Ord, Enum)
colors :: [Color]
colors = [(toEnum 0) ..]

newtype Player = Player
  { _name  :: T.Text
  } deriving (Show, Eq)
makeLenses ''Player

newtype Players = Players { toMap :: Map Color Player } deriving (Show, Eq)

fromList :: [(Color, Player)] -> Maybe Players
fromList ps | s >= 2 && s <= 4 = Just $ Players ps'
            | otherwise        = Nothing
 where
  ps' = Map.fromList ps
  s   = Map.size ps'

toList :: Players -> [(Color, Player)]
toList = Map.toList . toMap

next :: Player -> Players -> Player
next p ps = cycle l !! (i + 1)
 where
  i = fromJust $ L.elemIndex p l
  l = map snd $ toList ps
