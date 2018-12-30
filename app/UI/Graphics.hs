module UI.Graphics
  ( gate
  , tile
  , treasure
  , width
  , height
  )
where

import           Labyrinth.Game.Direction            ( Direction(..) )
import           Labyrinth.Game.Tile                 ( Terrain(..) )

width :: Int
width = 9

height :: Int
height = 4

gate :: Direction -> String
gate North = "         " ++
             "   ▲ ▲   " ++
             "         " ++
             "         "
gate West  = "         " ++
             "   ◄     " ++
             "   ◄     " ++
             "         "
gate South = "         " ++
             "         " ++
             "   ▼ ▼   " ++
             "         "
gate East  = "         " ++
             "     ►   " ++
             "     ►   " ++
             "         "

tile :: Terrain -> Direction -> String
tile Path   North = " │     │ " ++
                    " │     │ " ++
                    " │     │ " ++
                    " │     │ "
tile Path   West  = "─────────" ++
                    "         " ++
                    "         " ++
                    "─────────"
tile Path   South = " │     │ " ++
                    " │     │ " ++
                    " │     │ " ++
                    " │     │ "
tile Path   East  = "─────────" ++
                    "         " ++
                    "         " ++
                    "─────────"
tile Corner North = "─┘     │ " ++
                    "       │ " ++
                    "       │ " ++
                    "───────┘ "
tile Corner West  = "───────┐ " ++
                    "       │ " ++
                    "       │ " ++
                    "─┐     │ "
tile Corner South = " ┌───────" ++
                    " │       " ++
                    " │       " ++
                    " │     ┌─"
tile Corner East  = " │     └─" ++
                    " │       " ++
                    " │       " ++
                    " └───────"
tile Fork   North = "─┘     └─" ++
                    "         " ++
                    "         " ++
                    "─────────"
tile Fork   West  = "─┘     │ " ++
                    "       │ " ++
                    "       │ " ++
                    "─┐     │ "
tile Fork   South = "─────────" ++
                    "         " ++
                    "         " ++
                    "─┐     ┌─"
tile Fork   East  = " │     └─" ++
                    " │       " ++
                    " │       " ++
                    " │     ┌─"

treasure :: Char -> String
treasure c = "         " ++
             "         " ++
             "    " ++ [c] ++ "    " ++
             "         "
