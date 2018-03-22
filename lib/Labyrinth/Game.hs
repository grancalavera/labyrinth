{-# LANGUAGE TemplateHaskell #-}

module Labyrinth.Game
    ( Game (..)
    , Phase (..)
    , tileAt
    , playerAt
    , tiles
    , gates
    , rowSpread
    , colSpread
    , phase
    , initialGame
    , rotate
    , rotate'
    , move
    , donePlanning
    ) where

import           Control.Monad             (guard)
import qualified Data.List                 as List
import           Data.Map.Strict           (Map)
import qualified Data.Map.Strict           as Map
import           Data.Maybe                (fromMaybe)
import           Labyrinth                 (Position)
import           Labyrinth.Direction       (Direction (..))
import           Labyrinth.GameDescription (GameDescription (..),
                                            TileDescription (..), mkTiles)
import           Labyrinth.Gate            (Gate (..))
import           Labyrinth.Players         (Players)
import           Labyrinth.Tile            (Terrain (..), Tile (..))
import qualified Labyrinth.Tile            as Tile
import           Lens.Micro                ((&), (.~), (^.))
import           Lens.Micro.TH             (makeLenses)
import           Lens.Micro.Type           (Getting)

data Phase = Plan | Walk | End deriving (Show, Eq)

data Game = Game
    { _tileAt   :: Position
    , _playerAt :: Position
    , _tiles    :: Map Position Tile
    , _gates    :: Map Position Gate
    , _phase    :: Phase
    , _rowMin   :: Int
    , _rowMax   :: Int
    , _colMin   :: Int
    , _colMax   :: Int
    } deriving (Show, Eq)
makeLenses ''Game

initialGame :: Players -> IO Game
initialGame players' = do
  tiles'   <- mkTiles BD { _bTiles     = tiles''
                         , _bPlayers   = players'
                         , _bPositions = positions
                         }

  return $ Game
    { _tileAt   = startPosition
    , _playerAt = (-1,-1)
    , _gates    = Map.fromList gates'
    , _tiles    = Map.fromList tiles'
    , _phase    = Plan
    , _rowMin   = 0
    , _rowMax   = 8
    , _colMin   = 0
    , _colMax   = 8
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
      [ TD Corner (Just (1,1)) (Just South) False True  False False False
      , TD Fork   (Just (1,3)) (Just South) True  False False False False
      , TD Fork   (Just (1,5)) (Just South) True  False False False False
      , TD Corner (Just (1,7)) (Just West)  False False True  False False
      , TD Fork   (Just (3,1)) (Just East)  True  False False False False
      , TD Fork   (Just (3,3)) (Just East)  True  False False False False
      , TD Fork   (Just (3,5)) (Just South) True  False False False False
      , TD Fork   (Just (3,7)) (Just West)  True  False False False False
      , TD Fork   (Just (5,1)) (Just East)  True  False False False False
      , TD Fork   (Just (5,3)) (Just North) True  False False False False
      , TD Fork   (Just (5,5)) (Just West)  True  False False False False
      , TD Fork   (Just (5,7)) (Just West)  True  False False False False
      , TD Corner (Just (7,1)) (Just East)  False False False False True
      , TD Fork   (Just (7,3)) (Just North) True  False False False False
      , TD Fork   (Just (7,5)) (Just North) True  False False False False
      , TD Corner (Just (7,7)) (Just North) False False False True  False
      ]
      ++ (replicate 12 $ TD Path   Nothing Nothing False False False False False)
      ++ (replicate 6  $ TD Corner Nothing Nothing True  False False False False)
      ++ (replicate 10 $ TD Corner Nothing Nothing False False False False False)
      ++ (replicate 6  $ TD Fork   Nothing Nothing True  False False False False)
--------------------------------------------------------------------------------
-- state transitions
--------------------------------------------------------------------------------

donePlanning :: Game -> Game
donePlanning g = fromMaybe g $ do
  Gate _ isOpen <- Map.lookup (g ^. tileAt) (g ^. gates)
  guard isOpen
  return $ (nextPhase . toggleGates . updateCurrentTilePosition . slideTile) g

slideTile :: Game -> Game
slideTile g = g & tiles .~ (Map.mapKeys slide (g ^. tiles))
  where
    (cr, cc) = g ^. tileAt
    slide pos@(r, c) = fromMaybe pos $ do
      edge' <- edge g
      Just $ case edge' of
        North -> if (c==cc) then (r+1,c) else pos
        South -> if (c==cc) then (r-1,c) else pos
        West  -> if (r==cr) then (r,c+1) else pos
        East  -> if (r==cr) then (r,c-1) else pos

updateCurrentTilePosition :: Game -> Game
updateCurrentTilePosition g = g & tileAt .~ (update (g ^. tileAt))
  where
    update pos@(r,c) = fromMaybe pos $ do
      edge' <- edge g
      Just $ case edge' of
        North -> (g ^. rowMax, c)
        South -> (g ^. rowMin, c)
        West  -> (r, g ^. colMax)
        East  -> (r, g ^. colMin)

nextPhase :: Game -> Game
nextPhase = id
-- nextPhase g = g & phase .~ Walk

toggleGates :: Game -> Game
toggleGates g = g & gates .~ (Map.mapWithKey toggleGate (g ^. gates))
  where
    toggleGate pos (Gate dir _)
      | pos == g ^. tileAt = Gate dir False
      | otherwise          = Gate dir True

--------------------------------------------------------------------------------
-- Plan phase
--------------------------------------------------------------------------------

rotate :: Game -> Game
rotate = rotateInternal Tile.rotate

rotate' :: Game -> Game
rotate' = rotateInternal Tile.rotate'

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
  e <- edge g
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
-- etc
--------------------------------------------------------------------------------

edge :: Game -> Maybe Direction
edge g
  | r == c             = Nothing --  because we don't care about corners
  | r == (g ^. rowMin) = Just North
  | r == (g ^. rowMax) = Just South
  | c == (g ^. colMin) = Just West
  | c == (g ^. colMax) = Just East
  | otherwise          = Nothing
  where
    (r, c) = g ^. tileAt

rowSpread :: Game -> [Int]
rowSpread = spread rowMin rowMax

colSpread :: Game -> [Int]
colSpread = spread colMin colMax

spread :: Getting Int Game Int -> Getting Int Game Int -> Game -> [Int]
spread minLens maxLens g = [(g ^. minLens)..(g ^. maxLens)]
