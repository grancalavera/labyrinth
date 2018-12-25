module Labyrinth.Widgets.PlayerForm
  ( PlayerFormOptions
  , PlayerForm
  , playerForm
  , playerFormOptions
  )
where

import           Brick
import           Brick.Forms                    ( Form
                                                , FormFieldState
                                                , newForm
                                                , radioField
                                                , editTextField
                                                , (@@=)
                                                )
import           Lens.Micro.TH                  ( makeLenses )
import           Lens.Micro                     ( (^.) )
import           Data.Text                      ( Text )
import qualified Data.Text                     as Text
import qualified Data.Map.Strict               as Map
import           Data.Map.Strict                ( Map
                                                , (!)
                                                )
import           Labyrinth.Players              ( Player(..)
                                                , Color(..)
                                                , Players
                                                )
import qualified Labyrinth.Players             as Player
import           Labyrinth.Widgets.Internal     ( ResourceName(..) )

data PlayerFormOptions = PlayerFormOptions
  { _availableColors :: [Color] }
makeLenses ''PlayerFormOptions

type ColorFieldMap = Map Color ResourceName
type PlayerForm e = Form Player e ResourceName

playerForm :: PlayerFormOptions -> PlayerForm e
playerForm options =
  newForm [nameField, colorField options] $ defaultPlayer options

playerFormOptions :: Players -> PlayerFormOptions
playerFormOptions ps = PlayerFormOptions (Player.freeColors ps)

-- this is actually not safe...
defaultPlayer :: PlayerFormOptions -> Player
defaultPlayer = Player "" . head . (^. availableColors)

nameField :: Player -> FormFieldState Player e ResourceName
nameField = label "Name" @@= editTextField Player.name NameField (Just 1)

colorField
  :: PlayerFormOptions -> Player -> FormFieldState Player e ResourceName
colorField options =
  label "Color" @@= radioField Player.color (colorOptions options colorFieldMap)

label :: String -> Widget n -> Widget n
label s w =
  padBottom (Pad 1) $ (vLimit 1 $ hLimit 15 $ str s <+> fill ' ') <+> w

colorOptions
  :: PlayerFormOptions -> ColorFieldMap -> [(Color, ResourceName, Text)]
colorOptions options fieldsMap = zip3 colors fields labels
 where
  colors = options ^. availableColors
  labels = map (Text.pack . show) colors
  fields = map (\k -> fieldsMap ! k) colors

colorFieldMap :: ColorFieldMap
colorFieldMap = Map.fromList
  [ (Yellow, YellowField)
  , (Red   , RedField)
  , (Blue  , BlueField)
  , (Green , GreenField)
  ]
