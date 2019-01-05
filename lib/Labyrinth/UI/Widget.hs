module Labyrinth.UI.Widget
  ( page
  , paragraph
  , appContainer
  , titleBox
  , box
  , line
  , playerLabel
  , dialog
  , label
  )
where

import           Data.Text                      ( Text )
import           Brick
import qualified Brick.Widgets.Border          as B
import qualified Brick.Widgets.Center          as C
import           Lens.Micro                     ( (^.) )

import           Labyrinth.Game.Configuration   ( Player
                                                , name
                                                , color
                                                )

page :: [Text] -> Widget n
page = vBox . map paragraph

paragraph :: Text -> Widget n
paragraph = padBottom (Pad 1) . txtWrap

line :: Text -> Widget n
line t = vLimit 1 $ txt t <+> fill ' '

appContainer :: Int -> Widget n -> Widget n
appContainer w = C.vCenter . C.hCenter . hLimit w

titleBox :: String -> Widget n -> Widget n
titleBox s = B.borderWithLabel (str s) . padTop (Pad 1)

box :: Widget n -> Widget n
box = B.border . padTop (Pad 1)

playerLabel :: Int -> Player -> Widget n
playerLabel w p = playerAttr p $ hLimit w $ C.hCenter $ txt $ (p ^. name)

playerAttr :: Player -> Widget n -> Widget n
playerAttr = withAttr . attrName . show . (^. color)

dialog :: Widget n
dialog =
  C.centerLayer $ B.border $ padAll 4 $ txt "Layer floating \nOn top of it all."

label :: String -> Widget n -> Widget n
label s w = padBottom (Pad 1) $ vLimit 1 (hLimit 15 $ str s <+> fill ' ') <+> w
