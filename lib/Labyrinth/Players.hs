module Labyrinth.Players
  ( Color(..)
  , Player(..)
  , Players
  , colors
  , name
  , color
  , empty
  , toMap
  , toList
  , fromList
  , add
  , hasEnoughPlayers
  , freeColors
  , players
  , minPlayers
  , maxPlayers
  )
where

import           Data.Map.Strict                ( Map )
import qualified Data.Map.Strict               as Map
import qualified Data.Text                     as T
import qualified Data.Set                      as Set
import           Lens.Micro.TH                  ( makeLenses )
import           Lens.Micro                     ( (^.)
                                                , (&)
                                                , (.~)
                                                )

data Color = Yellow | Red  | Blue | Green deriving (Show, Eq, Ord, Enum)

colors :: [Color]
colors = [(toEnum 0) ..]

data Player = Player
  { _name  :: T.Text
  , _color :: Color
  } deriving (Show, Eq)
makeLenses ''Player

type PlayerMap = Map Color Player

data Players = Players
  { _players :: Map Color Player
  , _hasEnoughPlayers :: Bool
  , _minPlayers :: Int
  , _maxPlayers :: Int
  } deriving (Show, Eq)
makeLenses ''Players

empty :: Players
empty = Players mempty False 2 (length colors)

toMap :: Players -> PlayerMap
toMap ps = ps ^. players

add :: Players -> Player -> Players
add ps p = ps & players .~ players' & hasEnoughPlayers .~ hasEnoughPlayers'
 where
  players'          = Map.insert (p ^. color) p (toMap ps)
  hasEnoughPlayers' = ps ^. minPlayers <= Map.size players'

fromList :: [(Color, Player)] -> Maybe Players
fromList _ = Just empty

toList :: Players -> [(Color, Player)]
toList = Map.toList . toMap

freeColors :: Players -> [Color]
freeColors ps = Set.toList $ Set.difference existing taken
 where
  existing = Set.fromList colors
  taken    = Set.fromList $ Map.keys $ ps ^. players

-- fromList :: [(Color, Player)] -> Maybe Players
-- fromList ps | s >= 2 && s <= 4 = Just $ Players ps'
--             | otherwise        = Nothing
--  where
--   ps' = Map.fromList ps
--   s   = Map.size ps'

-- next :: Player -> Players -> Player
-- next p ps = cycle l !! (i + 1)
--  where
--   i = fromJust $ L.elemIndex p l
--   l = map snd $ toList ps
