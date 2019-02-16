module Labyrinth.Game.NewGame
  ( TileD
  , HasTreasure
  , availablePositions
  , addPositions
  , addPlayers
  , chooseDirections
  , addTreasures
  , newGame
  )
where
import           Control.Monad                  ( guard
                                                , forM
                                                )
import           Data.List                      ( partition )
import           Data.Set                       ( Set )
import qualified Data.Set                      as Set
import qualified Data.Map                      as Map
import           Data.Map                       ( (!?) )
import           Data.Maybe                     ( fromJust
                                                , isJust
                                                )
import           Lens.Micro                     ( _1
                                                , _2
                                                , _3
                                                , _4
                                                , _5
                                                , (^.)
                                                , (.~)
                                                , (?~)
                                                , (&)
                                                )
import qualified Data.Random                   as Random
import           Labyrinth.Game.Board           ( Board(..) )
import           Labyrinth.Game.Cell            ( Terrain
                                                , Cell(..)
                                                , TileCell(..)
                                                , GateCell
                                                , mkCell
                                                )
import           Labyrinth.Game.Position        ( Position )
import           Labyrinth.Game.Treasure        ( Treasure )
import qualified Labyrinth.Game.Direction      as D
import           Labyrinth.Game.Direction       ( Direction )
import qualified Labyrinth.Game.Player         as P
import           Labyrinth.Game.Player          ( Players
                                                , Player
                                                , PlayOrder
                                                )
import           Labyrinth.Game.Class           ( Game(..) )

type HasTreasure = Bool
type TileD a b = (Terrain, Maybe Position, Maybe Direction, a, Maybe b)

newGame
  :: [TileD HasTreasure PlayOrder]
  -> Board GateCell
  -> Players
  -> Int
  -> Int
  -> [Treasure]
  -> Position
  -> Set Position
  -> IO (Maybe Game)
newGame tiles gates players rows cols treasures extraTile positions = do

  let aPosList =
        availablePositions positions $ Set.fromList $ map (^. _2) tiles

  shufPos            <- Random.shuffle aPosList
  shufTre            <- Random.shuffle treasures
  tilesWithDirection <- chooseDirections tiles

  return
    $   addPlayers players tilesWithDirection
    >>= addPositions shufPos
    >>= addTreasures shufTre
    >>= traverse mkTile
    >>= \ts -> P.first players >>= \playing -> do
          let cellBoard = Board $ Map.fromList ts
          Just $ Game { _players   = players
                      , _playing   = playing
                      , _extraTile = extraTile
                      , _rowCount  = rows
                      , _colCount  = cols
                      , _tiles     = cellBoard
                      , _gates     = gates
                      , _treasures = mempty
                      }

mkTile :: TileD (Maybe Treasure) Player -> Maybe (Position, Cell TileCell)
mkTile t = do
  let terrain   = t ^. _1
      mTreasure = t ^. _4

  position  <- t ^. _2
  direction <- t ^. _3
  player    <- t ^. _5

  let cellData = TileCell mTreasure (Set.fromList [player])
      cell     = mkCell terrain direction cellData
  return (position, cell)

availablePositions :: Set Position -> Set (Maybe Position) -> [Position]
availablePositions p mp =
  Set.toList $ Set.map fromJust $ Set.difference (Set.map Just p) mp

addPlayers :: Players -> [TileD a PlayOrder] -> Maybe [TileD a Player]
addPlayers ps ts = do
  guard (sameLength && uniqOrders)
  traverse (addPlayer ps) addP >>= Just . (map (_5 .~ Nothing) addNothing <>)
 where
  (addP, addNothing) = partition (isJust . (^. _5)) ts
  sameLength         = length addP == Map.size ps
  uniqOrders         = length addP == Set.size (Set.fromList orders)
  orders             = map (fromJust . (^. _5)) addP

addPlayer :: Players -> TileD a PlayOrder -> Maybe (TileD a Player)
addPlayer ps t = t ^. _5 >>= (ps !?) >>= Just . (\p -> t & _5 ?~ p)

addPositions :: [Position] -> [TileD a b] -> Maybe [TileD a b]
addPositions ps ts = do
  guard (length ps == length needsP)
  Just $ hasP <> zipWith (_2 ?~) ps needsP
  where (hasP, needsP) = partition (isJust . (^. _2)) ts

chooseDirections :: [TileD a b] -> IO [TileD a b]
chooseDirections ts = forM ts $ \t -> do
  d <- chooseDirection (t ^. _3)
  return $ t & _3 ?~ d

addTreasures
  :: [Treasure] -> [TileD HasTreasure b] -> Maybe [TileD (Maybe Treasure) b]
addTreasures trs tls = do
  guard (length withT == length trs)
  Just $ map (_4 .~ Nothing) noT <> zipWith (_4 ?~) trs withT
  where (withT, noT) = partition (^. _4) tls

chooseDirection :: Maybe Direction -> IO Direction
chooseDirection md | isJust md = return $ fromJust md
                   | otherwise = D.random
