module Labyrinth.Game.Players
  ( Color(..)
  , Player(..)
  , Players
  , colors
  , name
  , order
  , color
  , toMap
  , toList
  , fromList
  )
where

import           Data.Map.Strict                ( Map )
import           Data.List                      ( sortBy )
import           Data.Text                      ( Text )
import           Lens.Micro                     ( (^.) )
import           Lens.Micro.TH                  ( makeLenses )
import qualified Data.Map.Strict               as Map

data Color = Yellow | Red  | Blue | Green deriving (Show, Eq, Ord, Enum)

colors :: [Color]
colors = [(toEnum 0) ..]

data Player = Player
  { _name  :: Text
  , _color :: Color
  , _order :: Int
  } deriving (Show, Eq)
makeLenses ''Player

type Players = Map Color Player

toList :: Players -> [(Color, Player)]
toList = sortBy playOrder . Map.toList
 where
  playOrder :: (Color, Player) -> (Color, Player) -> Ordering
  playOrder x y | ord x < ord y = LT
                | ord x > ord y = GT
                | otherwise     = EQ
  ord = (^. order) . snd


-- from this point all needs to be removed
-- just left it here b/c otherwise it will
-- not compile and I don't want to fix it yet

type PlayerMap = Players

toMap :: Players -> PlayerMap
toMap = id

fromList :: [(Color, Player)] -> Maybe Players
fromList = undefined
