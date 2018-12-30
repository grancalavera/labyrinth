module Labyrinth.UI.Screen.Registration
  ( RegistrationScreen
  , form
  , players
  , initialScreen
  , draw
  , submit
  , register
  , hasEnoughPlayers
  , isFull
  , chooseCursor
  , hasValidName
  )
where

import           Brick
import qualified Brick.Widgets.Center          as C
import           Brick.Focus                    ( focusRingCursor )
import           Brick.Forms                    ( Form
                                                , FormFieldState
                                                , newForm
                                                , radioField
                                                , editTextField
                                                , renderForm
                                                , formState
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

import           Labyrinth.Game.Players         ( Player(..)
                                                , Color(..)
                                                , Players
                                                , name
                                                )
import qualified Labyrinth.Game.Players        as Players
import           Labyrinth.UI.Widget
import           Labyrinth.UI.Internal          ( ResourceName(..) )

type RegistrationForm e = Form Player e ResourceName
type ColorFieldMap = Map Color ResourceName

data RegistrationScreen e = RegistrationScreen
  { _form :: Maybe (RegistrationForm e)
  , _players :: Players
  , _minPlayers :: Int
  , _maxPlayers :: Int
  }
makeLenses ''RegistrationScreen

initialScreen :: RegistrationScreen e
initialScreen = RegistrationScreen { _form       = mkForm mempty
                                   , _players    = mempty
                                   , _minPlayers = 2
                                   , _maxPlayers = 4
                                   }

draw :: RegistrationScreen s -> [Widget ResourceName]
draw screen = [appContainer 50 $ content]

 where
  content      = registration <=> registered <=> help

  registration = case screen ^. form of
    Just form' -> titleBox " Add Player " $ renderForm form'
    Nothing    -> emptyWidget

  registered = case (Players.toList $ screen ^. players) of
    [] -> emptyWidget
    ps -> titleBox " Players " $ vBox $ map toPlayer ps
  toPlayer      = playerLabel 50 . snd

  help          = beginCommand <=> submitCommand

  submitCommand = if hasValidName screen
    then C.hCenter $ txt "Ctrl+a: add player"
    else emptyWidget

  beginCommand = if hasEnoughPlayers screen
    then C.hCenter $ txt "Ctrl+p: begin game"
    else emptyWidget


submit :: RegistrationScreen e -> RegistrationScreen e
submit screen = maybe screen (register screen . formState) (screen ^. form)

register :: RegistrationScreen e -> Player -> RegistrationScreen e
register screen player = screen'
 where
  players' = Map.insert (player ^. Players.color) player (screen ^. players)
  form'    = mkForm players'
  screen'  = screen & form .~ form' & players .~ players'

mkForm :: Players -> Maybe (RegistrationForm e)
mkForm players' = case nextFormState players' of
  Just player' -> Just $ newForm [nameField, colorField players'] player'
  Nothing      -> Nothing

nextFormState :: Players -> Maybe Player
nextFormState players' = case availableColors players' of
  []      -> Nothing
  colors' -> Just (Player "" (head colors') (length Players.colors - length colors'))

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

hasValidName :: RegistrationScreen e -> Bool
hasValidName screen = maybe False validate (screen ^. form)
  where validate = (0 <) . Text.length . (^. name) . formState

chooseCursor
  :: RegistrationScreen e
  -> Maybe
       ([CursorLocation ResourceName] -> Maybe (CursorLocation ResourceName))
chooseCursor screen = case (screen ^. form) of
  Nothing    -> Nothing
  Just form' -> Just (focusRingCursor formFocus form')
