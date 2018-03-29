{-# LANGUAGE NamedFieldPuns  #-}
{-# LANGUAGE RankNTypes      #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module Labyrinth.Game
    ( Game (..)
    , Phase (..)
    , player
    , tileAt
    , tiles
    , gates
    , rowSpread
    , colSpread
    , phase
    , initialGame
    , rotate
    , rotate'
    , move
    , done
    , walk
    , players
    ) where

import           Control.Monad             (guard, void)
import qualified Data.List                 as List
import           Data.Map.Strict           (Map)
import qualified Data.Map.Strict           as Map
import           Data.Maybe                (fromJust, fromMaybe)
import           Labyrinth                 (Position)
import qualified Labyrinth                 as Labyrinth
import           Labyrinth.Direction       (Direction (..))
import           Labyrinth.GameDescription (GameDescription (..),
                                            TileDescription (..))
import qualified Labyrinth.GameDescription as GD
import           Labyrinth.Gate            (Gate (..))
import           Labyrinth.Players         (Color (..), Player, Players)
import qualified Labyrinth.Players         as P
import           Labyrinth.Tile            (Terrain (..), Tile (..))
import qualified Labyrinth.Tile            as T
import           Lens.Micro                ((&), (.~), (^.), _1, _2)
import           Lens.Micro.TH             (makeLenses)
import           Lens.Micro.Type           (Lens')

data Phase = Plan | Walk | End deriving (Show, Eq)

data Game = Game
    { _tileAt  :: Position
    , _tiles   :: Map Position Tile
    , _gates   :: Map Position Gate
    , _phase   :: Phase
    , _rowMin  :: Int
    , _rowMax  :: Int
    , _colMin  :: Int
    , _colMax  :: Int
    , _player  :: Player
    , _players :: Players
    } deriving (Show, Eq)
makeLenses ''Game

initialGame :: Players -> IO Game
initialGame players' = do
  player' <- firstPlayer players'
  tiles'  <- GD.mkTiles GD { _bTiles     = tiles''
                           , _bPlayers   = players'
                           , _bPositions = positions
                           }

  return Game { _tileAt  = startPosition
              , _player  = player'
              , _players = players'
              , _gates   = Map.fromList gates'
              , _tiles   = Map.fromList tiles'
              , _phase   = Plan
              , _rowMin  = 0
              , _rowMax  = 8
              , _colMin  = 0
              , _colMax  = 8
              }

  where
    startPosition = (0,2)
    positions = startPosition:[(x,y) | x <- [1..7], y <- [1..7]]
    gates' =
      [ ((0,2), Gate South True)
      , ((0,4), Gate South True)
      , ((0,6), Gate South True)
      , ((2,0), Gate East True)
      , ((4,0), Gate East True)
      , ((6,0), Gate East True)
      , ((2,8), Gate West True)
      , ((4,8), Gate West True)
      , ((6,8), Gate West True)
      , ((8,2), Gate North True)
      , ((8,4), Gate North True)
      , ((8,6), Gate North True)
      ]
    tiles'' =
      [ TD Corner (Just (1,1)) (Just South) False (Just Yellow)
      , TD Fork   (Just (1,3)) (Just South) True  Nothing
      , TD Fork   (Just (1,5)) (Just South) True  Nothing
      , TD Corner (Just (1,7)) (Just West)  False (Just Red)
      , TD Fork   (Just (3,1)) (Just East)  True  Nothing
      , TD Fork   (Just (3,3)) (Just East)  True  Nothing
      , TD Fork   (Just (3,5)) (Just South) True  Nothing
      , TD Fork   (Just (3,7)) (Just West)  True  Nothing
      , TD Fork   (Just (5,1)) (Just East)  True  Nothing
      , TD Fork   (Just (5,3)) (Just North) True  Nothing
      , TD Fork   (Just (5,5)) (Just West)  True  Nothing
      , TD Fork   (Just (5,7)) (Just West)  True  Nothing
      , TD Corner (Just (7,1)) (Just East)  False (Just Green)
      , TD Fork   (Just (7,3)) (Just North) True  Nothing
      , TD Fork   (Just (7,5)) (Just North) True  Nothing
      , TD Corner (Just (7,7)) (Just North) False (Just Blue)
      ]
      ++ (replicate 12 $ TD Path   Nothing Nothing False Nothing)
      ++ (replicate 6  $ TD Corner Nothing Nothing True  Nothing)
      ++ (replicate 10 $ TD Corner Nothing Nothing False Nothing)
      ++ (replicate 6  $ TD Fork   Nothing Nothing True  Nothing)

--------------------------------------------------------------------------------
-- state transitions
--------------------------------------------------------------------------------

done :: Game -> Game
done g = case (g ^. phase) of
  Plan -> fromMaybe g $ do
    Gate _ isOpen <- Map.lookup (g ^. tileAt) (g ^. gates)
    guard isOpen
    return $ ( nextPhase
             . teleport
             . toggleGates
             . updateCurrentTilePosition
             . slideTile
             ) g
  _ -> (nextPhase . nextPlayer) g

slideTile :: Game -> Game
slideTile g = g & tiles .~ (Map.mapKeys slide (g ^. tiles))
  where
    (cr, cc) = g ^. tileAt
    slide pos@(r, c) = fromMaybe pos $ do
      edge' <- edge (g ^. tileAt) g
      Just $ case edge' of
        North -> if (c==cc) then (r+1,c) else pos
        South -> if (c==cc) then (r-1,c) else pos
        West  -> if (r==cr) then (r,c+1) else pos
        East  -> if (r==cr) then (r,c-1) else pos

teleport :: Game -> Game
teleport g = foldl f g
                $ concatMap (\(p, t) -> zip (repeat p) (t ^. T.players))
                $ Map.toList
                $ Map.intersection (g ^. tiles) (g ^. gates)
  where
    f g' (from, p) = fromMaybe g' $ do
      op <- oppositeEdge from g'
      pd <- padding op g'
      -- TODO: refactor to use Linear.V2
      let to = (op^._1+pd^._1,op^._2+pd^._2)
      return $ walk' from to p g'

updateCurrentTilePosition :: Game -> Game
updateCurrentTilePosition g = g & tileAt .~ (update (g ^. tileAt))
  where
    update pos@(r,c) = fromMaybe pos $ do
      edge' <- edge (g ^. tileAt) g
      Just $ case edge' of
        North -> (g ^. rowMax, c)
        South -> (g ^. rowMin, c)
        West  -> (r, g ^. colMax)
        East  -> (r, g ^. colMin)

nextPhase :: Game -> Game
nextPhase g = g & phase .~ (nextPhase' (g ^. phase))
  where
    nextPhase' Plan = Walk
    nextPhase' _ = Plan

toggleGates :: Game -> Game
toggleGates g = g & gates .~ (Map.mapWithKey toggleGate (g ^. gates))
  where
    toggleGate pos (Gate dir _)
      | pos == g ^. tileAt = Gate dir False
      | otherwise        = Gate dir True

nextPlayer :: Game -> Game
nextPlayer g = g & player .~ (P.next (g ^. player) (g ^. players))

--------------------------------------------------------------------------------
-- Plan phase
--------------------------------------------------------------------------------

rotate :: Game -> Game
rotate = rotateInternal T.rotate

rotate' :: Game -> Game
rotate' = rotateInternal T.rotate'

rotateInternal :: (Tile -> Tile) -> Game -> Game
rotateInternal r g = g & (tiles .~ (Map.adjust r (g ^.tileAt) (g ^. tiles)))

move :: Direction -> Game -> Game
move dir g = fromMoves moves' g
  where
    moves' = moves dir g

fromMoves :: [Position] -> Game -> Game
fromMoves []        g = g
fromMoves (newP:ps) g = fromMaybe (fromMoves ps g) $ do
  _ <- Map.lookup newP (g ^. gates)
  Just $ (updatePos . moveTile) g
  where
    moveTile  = tiles .~ (Map.mapKeys moveKey (g ^. tiles))
    updatePos = tileAt .~ newP
    moveKey p
      | p == g ^. tileAt = newP
      | otherwise        = p

moves :: Direction -> Game -> [Position]
moves dir g = fromMaybe [] $ do
  e <- edge (g ^. tileAt) g
  let (r, c) = g ^.tileAt
      rMin   = g ^. rowMin
      rMax   = g ^. rowMax
      cMin   = g ^. colMin
      cMax   = g ^. colMax

  Just $ case (e, dir) of
    (North, East) -> List.union
      [(rMin,i) | i <- [c+1..cMax]]
      [(i,cMax) | i <- [rMin..rMax]]

    (North, West) -> List.union
      [(rMin, i) | i <- reverse [cMin..c-1]]
      [(i, cMin) | i <- [rMin..rMax]]

    (South, East) -> List.union
      [(rMax, i) | i <- [c+1..cMax]]
      [(i, cMax) | i <- reverse [rMin..rMax]]

    (South, West) -> List.union
      [(rMax, i) | i <- reverse [cMin..c-1]]
      [(i, cMin) | i <- reverse [rMin..rMax]]

    (West, North) -> List.union
      [(i, cMin) | i <- reverse [rMin..r-1]]
      [(rMin, i) | i <- [cMin..cMax]]

    (West, South) -> List.union
      [(i, cMin) | i <- [r+1..rMax]]
      [(rMax, i) | i <- [cMin..cMax]]

    (East, North) -> List.union
      [(i, cMax) | i <- reverse [rMin..r-1]]
      [(rMin, i) | i <- reverse [cMin..cMax]]

    (East, South) -> List.union
      [(i, cMax) | i <- [r+1..rMax]]
      [(rMax, i) | i <- reverse [cMin..cMax]]

    (North, South) -> [(i, c) | i <- [r+1..rMax]]
    (West, East)   -> [(r, i) | i <- [c+1..cMax]]
    (South, North) -> [(i, c) | i <- reverse [rMin..r-1]]
    (East, West)   -> [(r, i) | i <- reverse [cMin..c-1]]
    _              -> [] --  ¯\_(ツ)_/¯

--------------------------------------------------------------------------------
-- Walk phase
--------------------------------------------------------------------------------

walk :: Direction -> Game -> Game
walk d g = fromMaybe g $ do
  let player' = g ^. player
      tiles'  = g ^. tiles
  from  <- playerPosition g (player' ^. P.color)

  let to = nextPos from d
  void $ Map.lookup to tiles'

  return $ walk' from to player' g

walk' :: Position -> Position -> Player -> Game -> Game
walk' from to player' g = g & tiles .~ (
    ( (Map.alter (removePlayer player') from)
    . (Map.alter (insertPlayer player') to)
    ) (g ^. tiles))

insertPlayer :: Player -> Maybe Tile -> Maybe Tile
insertPlayer p  mt = do
  t <- mt
  return $ t & T.players .~ (p:(t ^. T.players))

removePlayer :: Player -> Maybe Tile -> Maybe Tile
removePlayer p mt = do
  t <- mt
  return $ t & T.players .~ (filter (/= p) (t ^. T.players))

nextPos :: Position -> Direction -> Position
nextPos (r,c) East  = (r, c+1)
nextPos (r,c) West  = (r,c-1)
nextPos (r,c) North = (r-1, c)
nextPos (r,c) South = (r+1, c)

--------------------------------------------------------------------------------
-- etc
--------------------------------------------------------------------------------

edge :: Position -> Game -> Maybe Direction
edge (r,c) g
  | r == c             = Nothing --  because we don't care about corners
  | r == (g ^. rowMin) = Just North
  | r == (g ^. rowMax) = Just South
  | c == (g ^. colMin) = Just West
  | c == (g ^. colMax) = Just East
  | otherwise          = Nothing

oppositeEdge :: Position -> Game -> Maybe Position
oppositeEdge p@(r,c) g = do
  edge' <- edge p g
  return $ case edge' of
    North -> (g ^. rowMax, c)
    South -> (g ^. rowMin, c)
    East  -> (r, g ^. colMin)
    West  -> (r, g ^. colMax)

padding :: Position -> Game -> Maybe Position
padding p g = do
  edge' <- edge p g
  return $ case edge' of
    North -> (1,0)
    South -> (-1,0)
    East  -> (0,-1)
    West  -> (0,1)

rowSpread :: Game -> [Int]
rowSpread = spread rowMin rowMax

colSpread :: Game -> [Int]
colSpread = spread colMin colMax

spread :: Lens' Game Int -> Lens' Game Int -> Game -> [Int]
spread mn mx g = [(g ^. mn)..(g ^. mx)]

firstPlayer :: Players -> IO Player
firstPlayer p = (Labyrinth.randomElem $ P.toList p) >>= (return . fromJust)

playerMap :: Game -> Map Color (Position, Player)
playerMap g = foldl f mempty (Map.toList (g ^. tiles))
  where
    f m (p, t) = foldl (f' p) m (t ^. T.players)
    f' p m p' = Map.insert (p' ^. P.color) (p,p') m

playerMap' :: Game -> Map Position Player
playerMap' = (foldl f mempty) . Map.toList . playerMap
  where
    f :: Map Position Player -> (Color, (Position, Player)) -> Map Position Player
    f m (_, (k, v)) = Map.insert k v m

playerPosition :: Game -> Color -> Maybe Position
playerPosition g c = Map.lookup c (playerMap g) >>= (return.fst)
