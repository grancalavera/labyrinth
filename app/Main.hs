module Main
where

import           Labyrinth
import           UI.Players                     ( addPlayers )
import           UI.Game                        ( playGame )

main :: IO ()
main = addPlayers >>= initialGame >>= playGame

initialGame :: Players -> IO Game
initialGame players = do
  let startPosition = (0, 2)
  gameFromDescription DGame
    { _gPlayers       = players
    , _gStartPosition = startPosition
    , _gRowMin        = 0
    , _gRowMax        = 8
    , _gColMin        = 0
    , _gColMax        = 8
    , _gGates         = [ ((0, 2), Gate South True)
                        , ((0, 4), Gate South True)
                        , ((0, 6), Gate South True)
                        , ((2, 0), Gate East True)
                        , ((4, 0), Gate East True)
                        , ((6, 0), Gate East True)
                        , ((2, 8), Gate West True)
                        , ((4, 8), Gate West True)
                        , ((6, 8), Gate West True)
                        , ((8, 2), Gate North True)
                        , ((8, 4), Gate North True)
                        , ((8, 6), Gate North True)
                        ]
    , _gTiles = [ DTile Corner (Just (1, 1)) (Just South) False (Just Yellow)
                , DTile Fork   (Just (1, 3)) (Just South) True  Nothing
                , DTile Fork   (Just (1, 5)) (Just South) True  Nothing
                , DTile Corner (Just (1, 7)) (Just West)  False (Just Red)
                , DTile Fork   (Just (3, 1)) (Just East)  True  Nothing
                , DTile Fork   (Just (3, 3)) (Just East)  True  Nothing
                , DTile Fork   (Just (3, 5)) (Just South) True  Nothing
                , DTile Fork   (Just (3, 7)) (Just West)  True  Nothing
                , DTile Fork   (Just (5, 1)) (Just East)  True  Nothing
                , DTile Fork   (Just (5, 3)) (Just North) True  Nothing
                , DTile Fork   (Just (5, 5)) (Just West)  True  Nothing
                , DTile Fork   (Just (5, 7)) (Just West)  True  Nothing
                , DTile Corner (Just (7, 1)) (Just East)  False (Just Green)
                , DTile Fork   (Just (7, 3)) (Just North) True  Nothing
                , DTile Fork   (Just (7, 5)) (Just North) True  Nothing
                , DTile Corner (Just (7, 7)) (Just North) False (Just Blue)
                ]
      ++ replicate 12 (DTile Path Nothing Nothing False Nothing)
      ++ replicate 6  (DTile Corner Nothing Nothing True Nothing)
      ++ replicate 10 (DTile Corner Nothing Nothing False Nothing)
      ++ replicate 6  (DTile Fork Nothing Nothing True Nothing)
    , _gPositions = startPosition : [ (x, y) | x <- [1 .. 7], y <- [1 .. 7] ]
    }
