module Main
where

import qualified Labyrinth
import           Labyrinth                      ( Players
                                                , Game
                                                , DGame(..)
                                                , DTile(..)
                                                , Gate(..)
                                                , Direction(..)
                                                , Terrain(..)
                                                , Color(..)
                                                )
import           UI.Players                     ( addPlayers )
import           UI.Game                        ( playGame )

main :: IO ()
main = addPlayers >>= testGame >>= playGame

testGame :: Players -> IO Game
testGame players = do
  let startPosition = (0, 2)
  Labyrinth.gameFromDescription DGame
    { _gPlayers       = players
    , _gStartPosition = startPosition
    , _gTreasures     = []
    , _gRowMin        = 0
    , _gRowMax        = 5
    , _gColMin        = 0
    , _gColMax        = 4
    , _gGates         = []
    , _gTiles         = [ DTile Path   (Just (1, 1)) (Just North) False (Just Yellow)
                        , DTile Corner (Just (1, 2)) (Just North) False Nothing
                        , DTile Fork   (Just (1, 3)) (Just North) False Nothing

                        , DTile Path   (Just (2, 1)) (Just North) False (Just Red)
                        , DTile Corner (Just (2, 2)) (Just North) False Nothing
                        , DTile Fork   (Just (2, 3)) (Just North) False Nothing

                        , DTile Path   (Just (3, 1)) (Just North) False (Just Blue)
                        , DTile Corner (Just (3, 2)) (Just North) False Nothing
                        , DTile Fork   (Just (3, 3)) (Just North) False Nothing

                        , DTile Path   (Just (4, 1)) (Just North) False (Just Green)
                        , DTile Corner (Just (4, 2)) (Just North) False Nothing
                        , DTile Fork   (Just (4, 3)) (Just North) False Nothing
                        ]
    }

fastGame :: Players -> IO Game
fastGame players = do
  let startPosition = (0, 2)
  Labyrinth.gameFromDescription DGame
    { _gPlayers       = players
    , _gStartPosition = startPosition
    , _gTreasures     = take 12 Labyrinth.treasures
    , _gRowMin        = 0
    , _gColMin        = 0
    , _gRowMax        = 6
    , _gColMax        = 6
    , _gGates         = [ ((0, 2), Gate South True)
                        , ((0, 4), Gate South True)
                        , ((2, 0), Gate East True)
                        , ((4, 0), Gate East True)
                        , ((2, 6), Gate West True)
                        , ((4, 6), Gate West True)
                        , ((6, 2), Gate North True)
                        , ((6, 4), Gate North True)
                        ]
    , _gTiles         = [ DTile Corner (Just (1, 1)) (Just South) False (Just Yellow)
                        , DTile Corner (Just (1, 5)) (Just West)  False (Just Red)
                        , DTile Corner (Just (5, 1)) (Just East)  False (Just Green)
                        , DTile Corner (Just (5, 5)) (Just North) False (Just Blue)
                        , DTile Fork   (Just (1, 3)) (Just South) True  Nothing
                        , DTile Fork   (Just (3, 1)) (Just East)  True  Nothing
                        , DTile Fork   (Just (3, 5)) (Just West)  True  Nothing
                        , DTile Fork   (Just (5, 3)) (Just North) True  Nothing
                        , DTile Fork   (Just (3, 3)) Nothing      True  Nothing
                        ]
      ++ replicate 5 (DTile Path Nothing Nothing False Nothing)
      ++ replicate 5 (DTile Corner Nothing Nothing False Nothing)
      ++ replicate 4 (DTile Corner Nothing Nothing True Nothing)
      ++ replicate 3 (DTile Fork Nothing Nothing True Nothing)
    }

regularGame :: Players -> IO Game
regularGame players = do
  let startPosition = (0, 2)
  Labyrinth.gameFromDescription DGame
    { _gPlayers       = players
    , _gStartPosition = startPosition
    , _gTreasures     = Labyrinth.treasures
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
    , _gTiles         = [ DTile Corner (Just (1, 1)) (Just South) False (Just Yellow)
                        , DTile Corner (Just (1, 7)) (Just West)  False (Just Red)
                        , DTile Corner (Just (7, 1)) (Just East)  False (Just Green)
                        , DTile Corner (Just (7, 7)) (Just North) False (Just Blue)
                        , DTile Fork   (Just (1, 3)) (Just South) True  Nothing
                        , DTile Fork   (Just (1, 5)) (Just South) True  Nothing
                        , DTile Fork   (Just (3, 1)) (Just East)  True  Nothing
                        , DTile Fork   (Just (5, 1)) (Just East)  True  Nothing
                        , DTile Fork   (Just (3, 7)) (Just West)  True  Nothing
                        , DTile Fork   (Just (5, 7)) (Just West)  True  Nothing
                        , DTile Fork   (Just (7, 3)) (Just North) True  Nothing
                        , DTile Fork   (Just (7, 5)) (Just North) True  Nothing
                        , DTile Fork   (Just (3, 3)) (Just East)  True  Nothing
                        , DTile Fork   (Just (5, 3)) (Just North) True  Nothing
                        , DTile Fork   (Just (3, 5)) (Just South) True  Nothing
                        , DTile Fork   (Just (5, 5)) (Just West)  True  Nothing
                        ]
      ++ replicate 12 (DTile Path Nothing Nothing False Nothing)
      ++ replicate 6  (DTile Corner Nothing Nothing True Nothing)
      ++ replicate 10 (DTile Corner Nothing Nothing False Nothing)
      ++ replicate 6  (DTile Fork Nothing Nothing True Nothing)
    }
