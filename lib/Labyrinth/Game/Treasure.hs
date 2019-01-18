module Labyrinth.Game.Treasure
  ( Treasure(..)
  , Treasures
  , TreasureMap
  , treasures
  , find
  , mapFromList
  , searchingList
  , foundList
  )
where

import           Control.Monad                  ( guard )
import qualified Data.Set                      as Set
import           Data.Set                       ( Set )
import           Lens.Micro.TH                  ( makeLenses )
import           Lens.Micro                     ( (^.) )
import qualified Data.Random                   as Random

data Treasure = TA | TB | TC | TD | TE | TF
              | TG | TH | TI | TJ | TK | TL
              | TM | TN | TO | TP | TQ | TR
              | TS | TT | TU | TV | TW | TX
              deriving (Show, Eq, Ord, Enum)

type Treasures = Set Treasure

data TreasureMap = TreasureMap
  { _searching :: Treasures
  , _found     :: Treasures
  } deriving(Show, Eq)
makeLenses ''TreasureMap

treasures :: [Treasure]
treasures = [(toEnum 0) ..]

find :: Treasure -> TreasureMap -> Maybe TreasureMap
find t ts = guard (Set.member t s) >> (Just $ TreasureMap s' f')
 where
  s  = ts ^. searching
  f  = ts ^. found
  s' = Set.delete t s
  f' = Set.insert t f

mapFromList :: [Treasure] -> TreasureMap
mapFromList ts = TreasureMap { _searching = Set.fromList ts, _found = mempty }

searchingList :: TreasureMap -> [Treasure]
searchingList = Set.toList . (^. searching)

foundList :: TreasureMap -> [Treasure]
foundList = Set.toList . (^. found)
