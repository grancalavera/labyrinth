module Labyrinth.Game.Players
  ( Color(..)
  , Player(..)
  , PlayOrder(..)
  , Players
  , PlayerIndex
  , initial
  , colors
  , name
  , order
  , color
  , players
  , minPlayers
  , maxPlayers
  , hasEnoughPlayers
  , isFull
  , toList
  , playerAt
  , insert
  , delete
  )
where

import           Data.Map.Strict                ( Map
                                                , (!?)
                                                )
import           Data.Text                      ( Text )
import           Lens.Micro                     ( (^.)
                                                , (%~)
                                                , (&)
                                                )
import           Lens.Micro.TH                  ( makeLenses )
import qualified Data.Map.Strict               as Map

data Color = Yellow | Red  | Blue | Green deriving (Show, Eq, Ord, Enum)
data PlayOrder = First | Second | Third | Fourth deriving (Show, Eq, Ord, Enum)
type PlayerIndex = Map PlayOrder Player

data Player = Player
  { _name  :: Text
  , _color :: Color
  , _order :: PlayOrder
  } deriving (Show, Eq)
makeLenses ''Player

data Players = Players
  { _players :: PlayerIndex
  , _minPlayers :: Int
  , _maxPlayers :: Int
  }
makeLenses ''Players

initial :: Players
initial = Players { _players = mempty, _minPlayers = 2, _maxPlayers = 4 }

colors :: [Color]
colors = [(toEnum 0) ..]

hasEnoughPlayers :: Players -> Bool
hasEnoughPlayers ps = (ps ^. minPlayers) <= count ps

isFull :: Players -> Bool
isFull ps = ps ^. maxPlayers == count ps

count :: Players -> Int
count = Map.size . (^. players)

toList :: Players -> [(PlayOrder, Player)]
toList = Map.toList . (^. players)

playerAt :: Players -> PlayOrder -> Maybe Player
playerAt ps = ((ps ^. players) !?)

insert :: Player -> Players -> Players
insert p ps = ps & players %~ Map.insert (p ^. order) p

delete :: Player -> Players -> Players
delete p ps = ps & players %~ Map.delete (p ^. order)
