module Labyrinth.Game.Players
  ( Player(..)
  , Color(..)
  , PlayOrder(..)
  , Players
  , name
  , color
  , order
  , colors
  , count
  , toList
  )
where

import           Lens.Micro.TH                  ( makeLenses )
import qualified Data.Map.Strict               as Map
import           Data.Map.Strict                ( Map )
import           Data.Text                      ( Text )

data Color = Yellow | Red  | Blue | Green deriving (Show, Eq, Ord, Enum)
data PlayOrder = First | Second | Third | Fourth deriving (Show, Eq, Ord, Enum)
type Players = Map PlayOrder Player

data Player = Player
  { _name  :: Text
  , _color :: Color
  , _order :: PlayOrder
  } deriving (Show, Eq)
makeLenses ''Player

colors :: [Color]
colors = [(toEnum 0) ..]

count :: Players -> Int
count = Map.size

toList :: Players -> [Player]
toList = map snd . Map.toList
