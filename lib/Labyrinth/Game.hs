{-# LANGUAGE TemplateHaskell #-}
module Labyrinth.Game
    ( Game (..)
    , currentPlayer
    , currentTilePosition
    , players
    , tiles
    , gates
    , rowSpread
    , colSpread
    , nextPlayer
    , initialGame
    ) where

import qualified Data.Map.Strict           as Map
import           Data.Map.Strict           (Map)
import           Lens.Micro                ((^.), (&), (.~))
import           Lens.Micro.TH             (makeLenses)
import           Labyrinth                 (Position)
import qualified Labyrinth.Players         as Players
import           Labyrinth.Players         ( Player(..)
                                           , Color(..)
                                           , Players(..)
                                           )
import           Labyrinth.Direction       (Direction(..))
import           Labyrinth.Tile            ( Tile(..)
                                           , Terrain(..)
                                           )
import           Labyrinth.Gate            (Gate(..))
import           Labyrinth.GameDescription ( GameDescription(..)
                                           , TileDescription(..)
                                           , mkTiles
                                           )

data Game = Game
    { _currentPlayer       :: Maybe Player
    , _currentTilePosition :: Position
    , _players             :: Players
    , _tiles               :: Map Position Tile
    , _gates               :: Map Position Gate
    , _rowSpread           :: [Int]
    , _colSpread           :: [Int]
    } deriving (Show, Eq)
makeLenses ''Game

initialGame :: Players -> IO Game
initialGame players' = do
  tiles' <- mkTiles GD { _dTiles     = tiles''
                       , _dPlayers   = players'
                       , _dPositions = (2,0):[(x,y) | x <- [1..7], y <- [1..7]]
                       }
  currentPlayer' <- Players.first players'

  return $ Game
    { _currentPlayer       = currentPlayer'
    , _currentTilePosition = (2,0)
    , _players             = players'
    , _rowSpread           = [0..8]
    , _colSpread           = [0..8]
    , _gates               = Map.fromList gates'
    , _tiles               = Map.fromList tiles'
    }

  where
    gates'  =
      [ ((2,0), Gate South True)
      , ((4,0), Gate South True)
      , ((6,0), Gate South True)
      , ((0,2), Gate East True)
      , ((0,4), Gate East True)
      , ((0,6), Gate East True)
      , ((8,2), Gate West True)
      , ((8,4), Gate West True)
      , ((8,6), Gate West True)
      , ((2,8), Gate North True)
      , ((4,8), Gate North True)
      , ((6,8), Gate North True)
      ]
    tiles'' =
      [ TD Corner  (Just (1,1)) (Just South)  False (Just Yellow)
      , TD Fork    (Just (3,1)) (Just East)   True  Nothing
      , TD Fork    (Just (5,1)) (Just East)   True  Nothing
      , TD Corner  (Just (7,1)) (Just West)   False (Just Red)
      , TD Fork    (Just (1,3)) (Just East)   True  Nothing
      , TD Fork    (Just (3,3)) (Just East)   True  Nothing
      , TD Fork    (Just (5,3)) (Just South)  True  Nothing
      , TD Fork    (Just (7,3)) (Just West)   True  Nothing
      , TD Fork    (Just (1,5)) (Just East)   True  Nothing
      , TD Fork    (Just (3,5)) (Just North)  True  Nothing
      , TD Fork    (Just (5,5)) (Just West)   True  Nothing
      , TD Fork    (Just (7,5)) (Just West)   True  Nothing
      , TD Corner  (Just (1,7)) (Just East)   False (Just Green)
      , TD Fork    (Just (3,7)) (Just North)  True  Nothing
      , TD Fork    (Just (5,7)) (Just North)  True  Nothing
      , TD Corner  (Just (7,7)) (Just North)  False (Just Blue)
      ]
      ++ (replicate 12 $ TD Path   Nothing Nothing False Nothing)
      ++ (replicate 6  $ TD Corner Nothing Nothing True  Nothing)
      ++ (replicate 10 $ TD Corner Nothing Nothing False Nothing)
      ++ (replicate 6  $ TD Fork   Nothing Nothing True  Nothing)

nextPlayer :: Game -> Maybe Game
nextPlayer g = do
  currP <- g ^. currentPlayer
  nextP <- Players.next currP (g ^. players)
  return $ g & currentPlayer .~ (Just nextP)
