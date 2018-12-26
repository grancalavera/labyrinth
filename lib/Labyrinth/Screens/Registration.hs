module Labyrinth.Screens.Registration
  ( RegistrationScreen
  , chooseCursor
  , draw
  , form
  , hasEnoughPlayers
  , initialScreen
  , players
  , register
  , isFull
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
import           Lens.Micro                     ( (^.)
                                                , (.~)
                                                , (&)
                                                )
import           Data.Text                      ( Text )
import qualified Data.Text                     as Text
import qualified Data.Map.Strict               as Map
import qualified Data.Set                      as Set
import           Data.Map.Strict                ( Map
                                                , (!)
                                                )
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
  , _minPlayers :: Int
  , _maxPlayers :: Int
  }
makeLenses ''RegistrationScreen

draw :: RegistrationScreen s -> [Widget ResourceName]
draw screen = [C.vCenter $ C.hCenter f <=> C.hCenter help]
 where
  f    = B.border $ padTop (Pad 1) $ hLimit 50 $ renderForm (screen ^. form)
  help = padTop (Pad 1) $ B.borderWithLabel (str "Help") body
  body = str "Foo bar help"

initialScreen :: RegistrationScreen e
initialScreen = RegistrationScreen { _form       = mkForm mempty
                                   , _players    = mempty
                                   , _minPlayers = 2
                                   , _maxPlayers = 4
                                   }
chooseCursor
  :: RegistrationScreen e
  -> [CursorLocation ResourceName]
  -> Maybe (CursorLocation ResourceName)
chooseCursor screen = focusRingCursor formFocus (screen ^. form)

mkForm :: Players -> RegistrationForm e
mkForm players' =
  newForm [nameField, colorField players'] $ defaultPlayer players'

-- this isn't safe anymore, but it might be fine as long as
-- we don't offer it as an API
defaultPlayer :: Players -> Player
defaultPlayer = Player "" . head . availableColors

nameField :: Player -> FormFieldState Player e ResourceName
nameField = label "Name" @@= editTextField Players.name NameField (Just 1)

colorField :: Players -> Player -> FormFieldState Player e ResourceName
colorField players' = label "Color"
  @@= radioField Players.color (colorOptions players' colorFieldMap)

label :: String -> Widget n -> Widget n
label s w = padBottom (Pad 1) $ vLimit 1 (hLimit 15 $ str s <+> fill ' ') <+> w

colorOptions :: Players -> ColorFieldMap -> [(Color, ResourceName, Text)]
colorOptions players' fieldsMap = zip3 colors fields labels
 where
  colors = availableColors players'
  labels = map (Text.pack . show) colors
  fields = map (\k -> fieldsMap ! k) colors

colorFieldMap :: ColorFieldMap
colorFieldMap = Map.fromList
  [ (Yellow, YellowField)
  , (Red   , RedField)
  , (Blue  , BlueField)
  , (Green , GreenField)
  ]

availableColors :: Players -> [Color]
availableColors ps = Set.toList $ Set.difference existing taken
 where
  existing = Set.fromList Players.colors
  taken    = Set.fromList $ Map.keys ps

hasEnoughPlayers :: RegistrationScreen e -> Bool
hasEnoughPlayers screen = minCount <= currentCount
 where
  currentCount = Map.size $ screen ^. players
  minCount     = screen ^. minPlayers

isFull :: RegistrationScreen e -> Bool
isFull screen = maxCount == currentCount
 where
  currentCount = Map.size $ screen ^. players
  maxCount     = screen ^. maxPlayers

register :: RegistrationScreen e -> Player -> RegistrationScreen e
register screen player = screen'
 where
  players' = Map.insert (player ^. Players.color) player (screen ^. players)
  form'    = mkForm players'
  screen'  = screen & form .~ form' & players .~ players'