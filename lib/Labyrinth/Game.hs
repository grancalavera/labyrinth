{-# LANGUAGE TemplateHaskell #-}
module Labyrinth.Game
    ( Game (..)
    , Phase (..)
    , currentPlayer
    , currentTilePosition
    , players
    , tiles
    , gates
    , rowSpread
    , colSpread
    , nextPlayer
    , phase
    , initialGame
    , rotate
    , rotate'
    , move
    , donePlanning
    ) where

import qualified Data.Map.Strict           as Map
import qualified Data.List                 as List
import           Data.Map.Strict           (Map)
import           Data.Maybe                (fromMaybe)
import           Lens.Micro                ((^.), (&), (.~))
import           Lens.Micro.TH             (makeLenses)
import           Lens.Micro.Type           (Getting)
import           Labyrinth                 (Position)
import qualified Labyrinth.Players         as Players
import           Labyrinth.Players         ( Player(..)
                                           , Color(..)
                                           , Players(..)
                                           )
import           Labyrinth.Direction       (Direction(..))
import qualified Labyrinth.Tile            as Tile
import           Labyrinth.Tile            ( Tile(..)
                                           , Terrain(..)
                                           )
import           Labyrinth.Gate            (Gate(..))
import           Labyrinth.GameDescription ( GameDescription(..)
                                           , TileDescription(..)
                                           , mkTiles
                                           )

data Phase = Plan | Walk | Check | End deriving (Show, Eq)

data Game = Game
    { _currentPlayer       :: Maybe Player
    , _currentTilePosition :: Position
    , _players             :: Players
    , _tiles               :: Map Position Tile
    , _gates               :: Map Position Gate
    , _phase               :: Phase
    , _rowMin              :: Int
    , _rowMax              :: Int
    , _colMin              :: Int
    , _colMax              :: Int
    } deriving (Show, Eq)
makeLenses ''Game

--------------------------------------------------------------------------------
-- state transitions
--------------------------------------------------------------------------------

donePlanning :: Game -> Game
donePlanning g = g & phase .~ Walk

--------------------------------------------------------------------------------
-- game transformations
--------------------------------------------------------------------------------
rotate :: Game -> Game
rotate = rotateInt $ Tile.rotate

rotate' :: Game -> Game
rotate' = rotateInt $ Tile.rotate'

move :: Direction -> Game -> Game
move dir g = fromMoves (moves dir g) g

fromMoves :: [Position] -> Game -> Game
fromMoves []     g = g
fromMoves (newP:ps) g = fromMaybe (fromMoves ps g) $ do
  _ <- Map.lookup newP (g ^. gates)
  let oldP = g ^. currentTilePosition
  tile' <- Map.lookup oldP (g ^. tiles)
  Just $ (g & currentTilePosition .~ newP)
          & tiles .~ (Map.insert newP tile' (Map.delete oldP (g ^. tiles)))

moves :: Direction -> Game -> [Position]
moves dir g = fromMaybe [] $ do
  e <- edge g
  let (r,c) = g ^.currentTilePosition
      rMin = g ^. rowMin
      rMax = g ^. rowMax
      cMin = g ^. colMin
      cMax = g ^. colMax

  Just $ case (e, dir) of
    (North, East) -> List.union
      [(rMin,i) | i <- [c+1..cMax]]
      [(i,cMax) | i <- [rMin..rMax]]

    (North, West) -> List.union
      [(rMin, i) | i <- reverse [cMin..c-1]]
      [(i, cMin) | i <- [rMin..rMax]]

    (North, South) -> [(i, c) | i <- [r+1..rMax]]

    (South, East) -> List.union
      [(rMax, i) | i <- [c+1..cMax]]
      [(i, cMax) | i <- reverse [rMin..rMax]]

    (South, West) -> List.union
      [(rMax, i) | i <- reverse [cMin..c-1]]
      [(i, cMin) | i <- reverse [rMin..rMax]]

    (South, North) -> [(i, c) | i <- reverse [rMin..r-1]]

    (West, North) -> List.union
      [(i, cMin) | i <- reverse [rMin..r-1]]
      [(rMin, i) | i <- [cMin..cMax]]

    (West, South) -> List.union
      [(i, cMin) | i <- [r+1..rMax]]
      [(rMax, i) | i <- [cMin..cMax]]

    (West, East) -> [(r, i) | i <- [c+1..cMax]]

    (East, North) -> List.union
      [(i, cMax) | i <- reverse [rMin..r-1]]
      [(rMin, i) | i <- reverse [cMin..cMax]]

    (East, South) -> List.union
      [(i, cMax) | i <- [r+1..rMax]]
      [(rMax, i) | i <- reverse [cMin..cMax]]

    (East, West) -> [(r, i) | i <- reverse [cMin..c-1]]

    _ -> []

edge :: Game -> Maybe Direction
edge g
  | r == c = Nothing
  | r == (g ^. rowMin) = Just North
  | r == (g ^. rowMax) = Just South
  | c == (g ^. colMin) = Just West
  | c == (g ^. colMax) = Just East
  | otherwise = Nothing
  where
    (r, c) = g ^. currentTilePosition

rotateInt :: (Tile -> Tile) -> Game -> Game
rotateInt rotateInt' g = fromMaybe g $ do
  tile' <- Map.lookup pos tiles'
  return $ g & tiles .~ (Map.insert pos (rotateInt' tile') tiles')
  where
    tiles' = g ^. tiles
    pos    = g ^. currentTilePosition

nextPlayer :: Game -> Game
nextPlayer g = fromMaybe g $ do
  currP <- g ^. currentPlayer
  nextP <- Players.next currP (g ^. players)
  return $ g & currentPlayer .~ (Just nextP)

--------------------------------------------------------------------------------
-- etc
--------------------------------------------------------------------------------

rowSpread :: Game -> [Int]
rowSpread = spread rowMin rowMax

colSpread :: Game -> [Int]
colSpread = spread colMin colMax

spread :: Getting Int Game Int -> Getting Int Game Int -> Game -> [Int]
spread minLens maxLens g = [(g ^. minLens)..(g ^. maxLens)]

initialGame :: Players -> IO Game
initialGame players' = do
  tiles' <- mkTiles GD { _dTiles     = tiles''
                       , _dPlayers   = players'
                       , _dPositions = startPosition:[(x,y) | x <- [1..7], y <- [1..7]]
                       }
  currentPlayer' <- Players.first players'

  return $ Game
    { _currentPlayer       = currentPlayer'
    , _currentTilePosition = startPosition
    , _players             = players'
    , _gates               = Map.fromList gates'
    , _tiles               = Map.fromList tiles'
    , _phase               = Plan
    , _rowMin              = 0
    , _rowMax              = 8
    , _colMin              = 0
    , _colMax              = 8
    }

  where
    startPosition = (0,2)
    gates'  =
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
      [ TD Corner  (Just (1,1)) (Just South)  False (Just [Yellow])
      , TD Fork    (Just (1,3)) (Just East)   True  Nothing
      , TD Fork    (Just (1,5)) (Just East)   True  Nothing
      , TD Corner  (Just (1,7)) (Just West)   False (Just [Red])
      , TD Fork    (Just (3,1)) (Just East)   True  Nothing
      , TD Fork    (Just (3,3)) (Just East)   True  Nothing
      , TD Fork    (Just (3,5)) (Just South)  True  Nothing
      , TD Fork    (Just (3,7)) (Just West)   True  Nothing
      , TD Fork    (Just (5,1)) (Just East)   True  Nothing
      , TD Fork    (Just (5,3)) (Just North)  True  Nothing
      , TD Fork    (Just (5,5)) (Just West)   True  Nothing
      , TD Fork    (Just (5,7)) (Just West)   True  Nothing
      , TD Corner  (Just (7,1)) (Just East)   False (Just [Green])
      , TD Fork    (Just (7,3)) (Just North)  True  Nothing
      , TD Fork    (Just (7,5)) (Just North)  True  Nothing
      , TD Corner  (Just (7,7)) (Just North)  False (Just [Blue])
      ]
      ++ (replicate 12 $ TD Path   Nothing Nothing False Nothing)
      ++ (replicate 6  $ TD Corner Nothing Nothing True  Nothing)
      ++ (replicate 10 $ TD Corner Nothing Nothing False Nothing)
      ++ (replicate 6  $ TD Fork   Nothing Nothing True  Nothing)
