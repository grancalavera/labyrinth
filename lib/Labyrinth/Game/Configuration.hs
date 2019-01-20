module Labyrinth.Game.Configuration
  ( Configuration
  , initial
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

import           Lens.Micro                     ( (^.)
                                                , (%~)
                                                , (&)
                                                )
import           Lens.Micro.TH                  ( makeLenses )
import qualified Data.Map.Strict               as Map
import           Data.Map.Strict                ( (!?) )

import qualified Data.Set                      as Set
import qualified Labyrinth.Game.Player         as P
import           Labyrinth.Game.Player          ( Players
                                                , Player
                                                , PlayOrder
                                                , Color
                                                )

data Configuration = Conf
  { _players    :: Players
  , _minPlayers :: Int
  } deriving (Show, Eq)
makeLenses ''Configuration

initial :: Configuration
initial = Conf { _players = mempty, _minPlayers = 2 }

hasEnoughPlayers :: Configuration -> Bool
hasEnoughPlayers ps = (ps ^. minPlayers) <= countPlayers ps

countPlayers :: Configuration -> Int
countPlayers = P.count . (^. players)

toList :: Configuration -> [Player]
toList = P.toList . (^. players)

playerAt :: Configuration -> PlayOrder -> Maybe Player
playerAt ps = ((ps ^. players) !?)

insert :: Player -> Configuration -> Configuration
insert p ps = ps & players %~ Map.insert (p ^. P.order) p

delete :: Player -> Configuration -> Configuration
delete p ps = ps & players %~ Map.delete (p ^. P.order)

availableColors :: Configuration -> [Color]
availableColors ps = Set.toList available
 where
  available = Set.difference existing taken
  existing  = Set.fromList P.colors
  taken     = Set.fromList $ map (^. P.color) (toList ps)

firstPlayer :: Configuration -> Maybe Player
firstPlayer = P.first . (^. players)
