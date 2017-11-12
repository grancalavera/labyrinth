module Labyrinth.UI where

import Brick.Widgets.Core (str)
import Brick.Main         (simpleMain)
import Brick.Types        (Widget)
import Data.List          (intercalate)

main :: IO ()
main = simpleMain ui

ui :: Widget ()
ui = str $ intercalate "\n" [" │   └─",
                             " │     ",
                             " └─────"]
