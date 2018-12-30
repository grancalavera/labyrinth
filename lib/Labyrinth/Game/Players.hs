module Labyrinth.Game.Players
  ( Color(..)
  , Player(..)
  , Players
  , colors
  , name
  , color
  , toMap
  , toList
  , fromList
  )
where

import           Data.Map.Strict                ( Map )
import           Data.Text                      ( Text )
import           Lens.Micro.TH                  ( makeLenses )
import qualified Data.Map.Strict               as Map

data Color = Yellow | Red  | Blue | Green deriving (Show, Eq, Ord, Enum)

colors :: [Color]
colors = [(toEnum 0) ..]

data Player = Player
  { _name  :: Text
  , _color :: Color
  } deriving (Show, Eq)
makeLenses ''Player

type Players = Map Color Player

-- from this point all needs to be removed
-- just left it here b/c otherwise it will
-- not compile and I don't want to fix it yet

type PlayerMap = Players

toMap :: Players -> PlayerMap
toMap = id

toList :: Players -> [(Color, Player)]
toList = Map.toList

fromList :: [(Color, Player)] -> Maybe Players
fromList = undefined
