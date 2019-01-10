module Labyrinth.Game.Configuration
  ( Configuration
  , Color(..)
  , Player(..)
  , PlayOrder(..)
  , Players
  , initial
  , colors
  , name
  , order
  , color
  , players
  , minPlayers
  , hasEnoughPlayers
  , toList
  , playerAt
  , insert
  , delete
  , availableColors
  , firstPlayer
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
import qualified Data.Set                      as Set

data Color = Yellow | Red  | Blue | Green deriving (Show, Eq, Ord, Enum)
data PlayOrder = First | Second | Third | Fourth deriving (Show, Eq, Ord, Enum)
type Players = Map PlayOrder Player

data Player = Player
  { _name  :: Text
  , _color :: Color
  , _order :: PlayOrder
  } deriving (Show, Eq)
makeLenses ''Player

data Configuration = Conf
  { _players    :: Players
  , _minPlayers :: Int
  } deriving (Show, Eq)
makeLenses ''Configuration

initial :: Configuration
initial = Conf { _players = mempty, _minPlayers = 2 }

colors :: [Color]
colors = [(toEnum 0) ..]

hasEnoughPlayers :: Configuration -> Bool
hasEnoughPlayers ps = (ps ^. minPlayers) <= countPlayers ps

countPlayers :: Configuration -> Int
countPlayers = Map.size . (^. players)

toList :: Configuration -> [Player]
toList = map snd . Map.toList . (^. players)

playerAt :: Configuration -> PlayOrder -> Maybe Player
playerAt ps = ((ps ^. players) !?)

insert :: Player -> Configuration -> Configuration
insert p ps = ps & players %~ Map.insert (p ^. order) p

delete :: Player -> Configuration -> Configuration
delete p ps = ps & players %~ Map.delete (p ^. order)

availableColors :: Configuration -> [Color]
availableColors ps = Set.toList available
 where
  available = Set.difference existing taken
  existing  = Set.fromList colors
  taken     = Set.fromList $ map (^. color) (toList ps)

firstPlayer :: Configuration -> Maybe Player
firstPlayer = (`playerAt` First)
