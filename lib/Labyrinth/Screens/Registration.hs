module Labyrinth.Screens.Registration
  ( mkForm
  , draw
  , optionsFromPlayers
  , chooseCursor
  , initialScreen
  , form
  , players
  , RegistrationFormOptions
  , RegistrationScreen(..)
  )
where

import           Brick
import qualified Brick.Widgets.Border          as B
import qualified Brick.Widgets.Center          as C
import           Brick.Focus                    ( focusRingCursor )
import           Brick.Forms                    ( Form
                                                , FormFieldState
                                                , newForm
                                                , radioField
                                                , editTextField
                                                , renderForm
                                                , formFocus
                                                , (@@=)
                                                )
import           Lens.Micro.TH                  ( makeLenses )
import           Lens.Micro                     ( (^.) )
import           Data.Text                      ( Text )
import qualified Data.Text                     as Text
import qualified Data.Map.Strict               as Map
import qualified Data.List.NonEmpty            as NonEmpty
import           Data.List.NonEmpty             ( NonEmpty(..) )
import           Data.Map.Strict                ( Map
                                                , (!)
                                                )
import           Data.Maybe                     ( fromJust )
import           Labyrinth.Players              ( Player(..)
                                                , Color(..)
                                                , Players
                                                )
import qualified Labyrinth.Players             as Players
import           Labyrinth.Screens.Internal     ( ResourceName(..) )

type RegistrationForm e = Form Player e ResourceName

type ColorFieldMap = Map Color ResourceName

data RegistrationScreen e = RegistrationScreen
  { _form :: RegistrationForm e
  , _players :: Players
  }
makeLenses ''RegistrationScreen

data RegistrationFormOptions = RegistrationFormOptions
  { _availableColors :: NonEmpty Color }
makeLenses ''RegistrationFormOptions

draw :: RegistrationScreen s -> [Widget ResourceName]
draw screen = [C.vCenter $ C.hCenter f <=> C.hCenter help]
 where
  f    = B.border $ padTop (Pad 1) $ hLimit 50 $ renderForm (screen ^. form)
  help = padTop (Pad 1) $ B.borderWithLabel (str "Help") body
  body = str "Foo bar help"

initialScreen :: RegistrationScreen e
initialScreen = RegistrationScreen (mkForm options) ps
 where
  ps      = Players.empty
  options = fromJust $ optionsFromPlayers ps

chooseCursor
  :: RegistrationScreen e
  -> [CursorLocation ResourceName]
  -> Maybe (CursorLocation ResourceName)
chooseCursor screen = focusRingCursor formFocus (screen ^. form)

mkForm :: RegistrationFormOptions -> RegistrationForm e
mkForm options =
  newForm [nameField, colorField options] $ defaultPlayer options

optionsFromPlayers :: Players -> Maybe RegistrationFormOptions
optionsFromPlayers ps = case (Players.freeColors ps) of
  [] -> Nothing
  cs -> Just $ RegistrationFormOptions (NonEmpty.fromList cs)

defaultPlayer :: RegistrationFormOptions -> Player
defaultPlayer = Player "" . NonEmpty.head . (^. availableColors)

nameField :: Player -> FormFieldState Player e ResourceName
nameField = label "Name" @@= editTextField Players.name NameField (Just 1)

colorField
  :: RegistrationFormOptions -> Player -> FormFieldState Player e ResourceName
colorField options = label "Color"
  @@= radioField Players.color (colorOptions options colorFieldMap)

label :: String -> Widget n -> Widget n
label s w = padBottom (Pad 1) $ vLimit 1 (hLimit 15 $ str s <+> fill ' ') <+> w

colorOptions
  :: RegistrationFormOptions -> ColorFieldMap -> [(Color, ResourceName, Text)]
colorOptions options fieldsMap = zip3 colors fields labels
 where
  colors = NonEmpty.toList $ options ^. availableColors
  labels = map (Text.pack . show) colors
  fields = map (\k -> fieldsMap ! k) colors

colorFieldMap :: ColorFieldMap
colorFieldMap = Map.fromList
  [ (Yellow, YellowField)
  , (Red   , RedField)
  , (Blue  , BlueField)
  , (Green , GreenField)
  ]
