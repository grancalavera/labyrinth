module Labyrinth.Game.Players
  ( Color(..)
  , Player(..)
  , PlayOrder(..)
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
import           Data.Text                      ( Text )
import           Lens.Micro.TH                  ( makeLenses )
import qualified Data.Map.Strict               as Map

data Color = Yellow | Red  | Blue | Green deriving (Show, Eq, Ord, Enum)
data PlayOrder = First | Second | Third | Fourth deriving (Show, Eq, Ord, Enum)

colors :: [Color]
colors = [(toEnum 0) ..]

data Player = Player
  { _name  :: Text
  , _color :: Color
  , _order :: PlayOrder
  } deriving (Show, Eq)
makeLenses ''Player

type Players = Map Color Player

toList :: Players -> [(Color, Player)]
toList =  Map.toList

-- from this point all needs to be removed
-- just left it here b/c otherwise it will
-- not compile and I don't want to fix it yet

type PlayerMap = Players

toMap :: Players -> PlayerMap
toMap = id

fromList :: [(Color, Player)] -> Maybe Players
fromList = undefined
