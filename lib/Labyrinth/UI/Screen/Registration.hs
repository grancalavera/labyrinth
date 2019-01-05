module Labyrinth.UI.Screen.Registration
  ( RegistrationScreen
  , submitPlayer
  , validate
  , editPlayer
  , playerAt
  , processForm
  , hasEnoughPlayers
  , chooseCursor
  , extractPlayer
  , initial
  , draw
  --
  , form
  , register
  )
where

import           Brick
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
                                                , (?~)
                                                , (&)
                                                )
import           Data.Text                      ( Text )
import qualified Data.Text                     as Text
import qualified Data.Map.Strict               as Map
import           Data.Map.Strict                ( Map
                                                , (!)
                                                )

import           Labyrinth.Game.Configuration   ( Player(..)
                                                , Color(..)
                                                , Configuration
                                                , PlayOrder
                                                , name
                                                , order
                                                )
import qualified Labyrinth.Game.Configuration  as Conf
import           Labyrinth.UI.Widget
import           Labyrinth.UI.Internal

type ColorFieldMap = Map Color Name
type PlayerForm e = Form Player e Name
type FormProcessor e = PlayerForm e -> EventM Name (PlayerForm e)
data TheForm e = AddPlayerForm (PlayerForm e) | EditPlayerForm ( PlayerForm e)

data RegistrationScreen e = RegistrationScreen
  { _form :: Maybe (TheForm e)
  , _conf :: Configuration
  }
makeLenses ''RegistrationScreen

initial :: RegistrationScreen e
initial = RegistrationScreen { _form = mkAddPlayerForm Conf.initial
                             , _conf = Conf.initial
                             }

draw :: RegistrationScreen s -> Widget Name
draw screen = theForm <=> registered <=> help

 where
  theForm = case screen ^. form of
    Just (AddPlayerForm  form') -> titleBox " Add Player " $ renderForm form'
    Just (EditPlayerForm form') -> titleBox " Edit Player " $ renderForm form'
    _                           -> emptyWidget

  registered = case Conf.toList $ screen ^. conf of
    [] -> emptyWidget
    ps -> titleBox " Players " $ vBox $ map (toPlayer . snd) ps

  toPlayer p = playerLabel 35 p <+> editPlayerCommand p

  help = submitCommand <=> beginCommand <=> quitCommand

  submitCommand =
    if validate screen then txt "Enter: add player" else emptyWidget

  beginCommand =
    if hasEnoughPlayers screen then txt "Ctrl+p: begin game" else emptyWidget

  editPlayerCommand p =
    str $ " " <> "Edit: Ctrl+" <> ["a", "s", "d", "f"] !! fromEnum (p ^. order)

  quitCommand = txt "Ctrl+q: quit"

submitPlayer :: RegistrationScreen e -> RegistrationScreen e
submitPlayer screen = maybe screen (register screen) (extractPlayer screen)

validate :: RegistrationScreen e -> Bool
validate screen = maybe False (val . extractForm) (screen ^. form)
  where val = (0 <) . Text.length . (^. name) . formState

editPlayer :: RegistrationScreen e -> Player -> RegistrationScreen e
editPlayer screen player =
  screen & form ?~ mkEditPlayerForm (screen ^. conf) player

playerAt :: RegistrationScreen e -> PlayOrder -> Maybe Player
playerAt screen = Conf.playerAt (screen ^. conf)

processForm
  :: RegistrationScreen e
  -> FormProcessor e
  -> EventM Name (RegistrationScreen e)
processForm screen process = case screen ^. form of
  Just (AddPlayerForm  form') -> processAndWrap asAdd form'
  Just (EditPlayerForm form') -> processAndWrap asEdit form'
  Nothing                     -> return screen
 where
  processAndWrap wrapAs f = do
    f' <- process f
    return $ screen & form ?~ wrapAs f'

extractPlayer :: RegistrationScreen e -> Maybe Player
extractPlayer screen = formState . extractForm <$> (screen ^. form)

extractForm :: TheForm e -> PlayerForm e
extractForm (AddPlayerForm  f) = f
extractForm (EditPlayerForm f) = f

asAdd :: PlayerForm e -> TheForm e
asAdd = AddPlayerForm

asEdit :: PlayerForm e -> TheForm e
asEdit = EditPlayerForm

register :: RegistrationScreen e -> Player -> RegistrationScreen e
register screen player = RegistrationScreen form' conf'
 where
  conf' = Conf.insert player (screen ^. conf)
  form' = mkAddPlayerForm conf'

mkAddPlayerForm :: Configuration -> Maybe (TheForm e)
mkAddPlayerForm cfg = case nextDefaultPlayer cfg of
  Just player -> Just $ AddPlayerForm (mkForm cfg player)
  Nothing     -> Nothing

mkEditPlayerForm :: Configuration -> Player -> TheForm e
mkEditPlayerForm cfg p = EditPlayerForm (mkForm (Conf.delete p cfg) p)

nextDefaultPlayer :: Configuration -> Maybe Player
nextDefaultPlayer cfg = case Conf.availableColors cfg of
  []      -> Nothing
  colors' -> Just
    (Player "" (head colors') (toEnum $ length Conf.colors - length colors'))

mkForm :: Configuration -> Player -> PlayerForm e
mkForm cfg = newForm [nameField, colorField cfg]

nameField :: Player -> FormFieldState Player e Name
nameField = label "Name" @@= editTextField Conf.name NameField (Just 1)

colorField :: Configuration -> Player -> FormFieldState Player e Name
colorField cfg =
  label "Color" @@= radioField Conf.color (colorOptions cfg colorFieldMap)

colorOptions :: Configuration -> ColorFieldMap -> [(Color, Name, Text)]
colorOptions cfg fieldsMap = zip3 colors fields labels
 where
  colors = Conf.availableColors cfg
  labels = map (Text.pack . show) colors
  fields = map (\k -> fieldsMap ! k) colors

colorFieldMap :: ColorFieldMap
colorFieldMap = Map.fromList
  [ (Yellow, YellowField)
  , (Red   , RedField)
  , (Blue  , BlueField)
  , (Green , GreenField)
  ]

hasEnoughPlayers :: RegistrationScreen e -> Bool
hasEnoughPlayers = Conf.hasEnoughPlayers . (^. conf)

chooseCursor
  :: RegistrationScreen e
  -> Maybe ([CursorLocation Name] -> Maybe (CursorLocation Name))
chooseCursor screen = case (screen ^. form) of
  Nothing    -> Nothing
  Just form' -> Just (focusRingCursor formFocus $ extractForm form')
