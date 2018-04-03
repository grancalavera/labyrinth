module Main
where

import           Linear.V2                      ( V2(..) )
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
main = addPlayers >>= regularGame >>= playGame

testGame :: Players -> IO Game
testGame players = Labyrinth.gameFromDescription DGame
    { _gPlayers       = players
    , _gStartPosition = V2 0 2
    , _gTreasures     = []
    , _gRows          = 6
    , _gCols          = 5
    , _gGates         = []
    , _gTiles         = [ DTile Path (Just $ V2 1 1) (Just North) False (Just Yellow)
                        , DTile Corner (Just $ V2 1 2) (Just North) False Nothing
                        , DTile Fork   (Just $ V2 1 3) (Just North) False Nothing
                        , DTile Path   (Just $ V2 2 1) (Just North) False (Just Red)
                        , DTile Corner (Just $ V2 2 2) (Just North) False Nothing
                        , DTile Fork   (Just $ V2 2 3) (Just North) False Nothing
                        , DTile Path   (Just $ V2 3 1) (Just North) False (Just Blue)
                        , DTile Corner (Just $ V2 3 2) (Just North) False Nothing
                        , DTile Fork   (Just $ V2 3 3) (Just North) False Nothing
                        , DTile Path (Just $ V2 4 1) (Just North) False (Just Green)
                        , DTile Corner (Just $ V2 4 2) (Just North) False Nothing
                        , DTile Fork   (Just $ V2 4 3) (Just North) False Nothing
                        ]
    }

regularGame :: Players -> IO Game
regularGame players = Labyrinth.gameFromDescription DGame
    { _gPlayers       = players
    , _gStartPosition = V2 0 2
    , _gTreasures     = Labyrinth.treasures
    , _gRows          = 9
    , _gCols          = 9
    , _gGates         = [ (V2 0 2, Gate South True)
                        , (V2 0 4, Gate South True)
                        , (V2 0 6, Gate South True)
                        , (V2 2 0, Gate East True)
                        , (V2 4 0, Gate East True)
                        , (V2 6 0, Gate East True)
                        , (V2 2 8, Gate West True)
                        , (V2 4 8, Gate West True)
                        , (V2 6 8, Gate West True)
                        , (V2 8 2, Gate North True)
                        , (V2 8 4, Gate North True)
                        , (V2 8 6, Gate North True)
                        ]
    , _gTiles = [ DTile Corner (Just $ V2 1 1) (Just South) False (Just Yellow)
                , DTile Corner (Just $ V2 1 7) (Just West)  False (Just Red)
                , DTile Corner (Just $ V2 7 1) (Just East)  False (Just Green)
                , DTile Corner (Just $ V2 7 7) (Just North) False (Just Blue)
                , DTile Fork   (Just $ V2 1 3) (Just South) True  Nothing
                , DTile Fork   (Just $ V2 1 5) (Just South) True  Nothing
                , DTile Fork   (Just $ V2 3 1) (Just East)  True  Nothing
                , DTile Fork   (Just $ V2 5 1) (Just East)  True  Nothing
                , DTile Fork   (Just $ V2 3 7) (Just West)  True  Nothing
                , DTile Fork   (Just $ V2 5 7) (Just West)  True  Nothing
                , DTile Fork   (Just $ V2 7 3) (Just North) True  Nothing
                , DTile Fork   (Just $ V2 7 5) (Just North) True  Nothing
                , DTile Fork   (Just $ V2 3 3) (Just East)  True  Nothing
                , DTile Fork   (Just $ V2 5 3) (Just North) True  Nothing
                , DTile Fork   (Just $ V2 3 5) (Just South) True  Nothing
                , DTile Fork   (Just $ V2 5 5) (Just West)  True  Nothing
                ]
        ++ replicate 12 (DTile Path Nothing Nothing False Nothing)
        ++ replicate 6  (DTile Corner Nothing Nothing True Nothing)
        ++ replicate 10 (DTile Corner Nothing Nothing False Nothing)
        ++ replicate 6  (DTile Fork Nothing Nothing True Nothing)
    }
