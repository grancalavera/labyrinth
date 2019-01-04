module Labyrinth.UI.Screen.Registration
  ( RegistrationScreen
  , submitPlayer
  , validate
  , editPlayer
  , playerAt
  , processForm
  , hasEnoughPlayers
  , form
  , players
  , initial
  , draw
  , register
  , isFull
  , chooseCursor
  , extractForm
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
                                                , (.~)
                                                , (?~)
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
                                                , PlayOrder
                                                , PlayerIndex
                                                , name
                                                , order
                                                )
import qualified Labyrinth.Game.Players        as Players
import           Labyrinth.UI.Widget
import           Labyrinth.UI.Internal

type ColorFieldMap = Map Color Name
type PlayerForm e = Form Player e Name
type FormProcessor e = PlayerForm e -> EventM Name (PlayerForm e)
data TheForm e = AddPlayer ( PlayerForm e) | EditPlayer ( PlayerForm e)

data RegistrationScreen e = RegistrationScreen
  { _form :: Maybe (TheForm e)
  , _players :: Players
  }
makeLenses ''RegistrationScreen

initial :: RegistrationScreen e
initial = RegistrationScreen { _form    = addPlayerForm mempty
                             , _players = Players.initial
                             }

submitPlayer :: RegistrationScreen e -> RegistrationScreen e
submitPlayer screen =
  maybe screen (register screen . formState . extractForm) (screen ^. form)

validate :: RegistrationScreen e -> Bool
validate screen = maybe False (val . extractForm) (screen ^. form)
  where val = (0 <) . Text.length . (^. name) . formState

editPlayer :: RegistrationScreen e -> Player -> RegistrationScreen e
editPlayer screen player =
  screen & form ?~ editPlayerForm (screen ^. players) player

playerAt :: RegistrationScreen e -> PlayOrder -> Maybe Player
playerAt screen = Players.playerAt (screen ^. players)

processForm
  :: RegistrationScreen e
  -> FormProcessor e
  -> EventM Name (RegistrationScreen e)
processForm screen process = case screen ^. form of
  Just (AddPlayer  form') -> processAndPack asAdd form'
  Just (EditPlayer form') -> processAndPack asEdit form'
  Nothing                 -> return screen
 where
  processAndPack packAs f = do
    f' <- process f
    return $ screen & form ?~ packAs f'

draw :: RegistrationScreen s -> Widget Name
draw screen = content

 where
  content = theForm <=> registered <=> help

  theForm = case screen ^. form of
    Just (AddPlayer  form') -> titleBox " Add Player " $ renderForm form'
    Just (EditPlayer form') -> titleBox " Edit Player " $ renderForm form'
    _                       -> emptyWidget

  registered = case (Players.toList $ screen ^. players) of
    [] -> emptyWidget
    ps -> titleBox " Players " $ vBox $ map (toPlayer . snd) ps

  toPlayer p = playerLabel 35 p <+> editPlayerLabel p

  help        = beginCommand <=> submitCommand <=> quitCommand

  quitCommand = txt "Ctrl+q: quit"

  submitCommand =
    if validate screen then txt "Enter: add player" else emptyWidget

  beginCommand =
    if hasEnoughPlayers screen then txt "Ctrl+p: begin game" else emptyWidget

  editPlayerLabel p =
    str
      $  " "
      <> "Edit: Ctrl+"
      <> ["a", "s", "d", "f"]
      !! (fromEnum $ p ^. order)

extractForm :: TheForm e -> PlayerForm e
extractForm (AddPlayer  f) = f
extractForm (EditPlayer f) = f

asAdd :: PlayerForm e -> TheForm e
asAdd = AddPlayer

asEdit :: PlayerForm e -> TheForm e
asEdit = EditPlayer

register :: RegistrationScreen e -> Player -> RegistrationScreen e
register screen player = screen'
 where
  players' = Players.insert player (screen ^. players)
  form'    = addPlayerForm (players' ^. Players.players)
  screen'  = screen & form .~ form' & players .~ players'

addPlayerForm :: PlayerIndex -> Maybe (TheForm e)
addPlayerForm players' = case nextFormState players' of
  Just player -> Just $ AddPlayer (mkForm players' player)
  Nothing     -> Nothing

editPlayerForm :: Players -> Player -> TheForm e
editPlayerForm ps p = EditPlayer (mkForm (ps' ^. Players.players) p)
  where ps' = Players.delete p ps

mkForm :: PlayerIndex -> Player -> PlayerForm e
mkForm ps = newForm [nameField, colorField ps]

nextFormState :: PlayerIndex -> Maybe Player
nextFormState players' = case availableColors players' of
  []      -> Nothing
  colors' -> Just
    (Player "" (head colors') (toEnum $ length Players.colors - length colors'))

nameField :: Player -> FormFieldState Player e Name
nameField = label "Name" @@= editTextField Players.name NameField (Just 1)

colorField :: PlayerIndex -> Player -> FormFieldState Player e Name
colorField players' = label "Color"
  @@= radioField Players.color (colorOptions players' colorFieldMap)

label :: String -> Widget n -> Widget n
label s w = padBottom (Pad 1) $ vLimit 1 (hLimit 15 $ str s <+> fill ' ') <+> w

colorOptions :: PlayerIndex -> ColorFieldMap -> [(Color, Name, Text)]
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

availableColors :: PlayerIndex -> [Color]
availableColors ps = Set.toList $ Set.difference existing taken
 where
  existing = Set.fromList Players.colors
  taken    = Set.fromList $ map ((^. Players.color) . snd) (Map.toList ps)

hasEnoughPlayers :: RegistrationScreen e -> Bool
hasEnoughPlayers = Players.hasEnoughPlayers . (^. players)

isFull :: RegistrationScreen e -> Bool
isFull = Players.isFull . (^. players)

chooseCursor
  :: RegistrationScreen e
  -> Maybe ([CursorLocation Name] -> Maybe (CursorLocation Name))
chooseCursor screen = case (screen ^. form) of
  Nothing    -> Nothing
  Just form' -> Just (focusRingCursor formFocus $ extractForm form')
