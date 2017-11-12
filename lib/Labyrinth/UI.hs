module Labyrinth.UI where

import Brick.Widgets.Core (str)
import Brick.Main         (simpleMain)
import Data.List          (intercalate)

main :: IO ()
main = simpleMain ui

ui :: Widget ()
ui = str $ intercalate "\n" [" │   └─",
                             " │     ",
                             " └─────"]
