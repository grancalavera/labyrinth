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
  , initialScreen
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
                                                , (!?)
                                                )

import           Labyrinth.Game.Players         ( Player(..)
                                                , Color(..)
                                                , name
                                                , order
                                                )
import qualified Labyrinth.Game.Players        as Players
import           Labyrinth.UI.Widget
import           Labyrinth.UI.Internal          ( ResourceName(..) )

type PlayerIndex = Map Int Player
type ColorFieldMap = Map Color ResourceName
type PlayerForm e = Form Player e ResourceName
type FormProcessor e = PlayerForm e -> EventM ResourceName (PlayerForm e)

data TheForm e = AddPlayer ( PlayerForm e) | EditPlayer ( PlayerForm e)

data RegistrationScreen e = RegistrationScreen
  { _form :: Maybe (TheForm e)
  , _players :: PlayerIndex
  , _minPlayers :: Int
  , _maxPlayers :: Int
  }
makeLenses ''RegistrationScreen

initialScreen :: RegistrationScreen e
initialScreen = RegistrationScreen { _form       = addPlayerForm mempty
                                   , _players    = mempty
                                   , _minPlayers = 2
                                   , _maxPlayers = 4
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

playerAt :: RegistrationScreen e -> Int -> Maybe Player
playerAt screen = ((screen ^. players) !?)

processForm
  :: RegistrationScreen e
  -> FormProcessor e
  -> EventM ResourceName (RegistrationScreen e)
processForm screen process = case screen ^. form of
  Just (AddPlayer  form') -> processAndPack asAdd form'
  Just (EditPlayer form') -> processAndPack asEdit form'
  Nothing                 -> return screen
 where
  processAndPack packAs f = do
    f' <- process f
    return $ screen & form ?~ packAs f'

draw :: RegistrationScreen s -> [Widget ResourceName]
draw screen = [appContainer 50 $ content]

 where
  content = theForm <=> registered <=> help

  theForm = case screen ^. form of
    Just (AddPlayer  form') -> titleBox " Add Player " $ renderForm form'
    Just (EditPlayer form') -> titleBox " Edit Player " $ renderForm form'
    _                       -> emptyWidget

  registered = case (Map.toList $ screen ^. players) of
    [] -> emptyWidget
    ps -> titleBox " Players " $ vBox $ map (toPlayer . snd) ps

  toPlayer p = playerLabel 35 p <+> editPlayerLabel p

  help        = beginCommand <=> submitCommand <=> quitCommand

  quitCommand = txt "Ctrl+q: quit"

  submitCommand =
    if validate screen then txt "Enter: add player" else emptyWidget

  beginCommand =
    if hasEnoughPlayers screen then txt "Ctrl+p: begin game" else emptyWidget

  editPlayerLabel p = str $ " " <> "Edit: Ctrl+" <> ["a", "s", "d", "f"] !!  (p ^. order)

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
  players' = Map.insert (player ^. Players.order) player (screen ^. players)
  form'    = addPlayerForm players'
  screen'  = screen & form .~ form' & players .~ players'

addPlayerForm :: PlayerIndex -> Maybe (TheForm e)
addPlayerForm players' = case nextFormState players' of
  Just player -> Just $ AddPlayer (mkForm players' player)
  Nothing     -> Nothing

editPlayerForm :: PlayerIndex -> Player -> TheForm e
editPlayerForm ps p = EditPlayer (mkForm ps' p)
  where ps' = Map.delete (p ^. Players.order) ps

mkForm :: PlayerIndex -> Player -> PlayerForm e
mkForm ps = newForm [nameField, colorField ps]

nextFormState :: PlayerIndex -> Maybe Player
nextFormState players' = case availableColors players' of
  [] -> Nothing
  colors' ->
    Just (Player "" (head colors') (length Players.colors - length colors'))

nameField :: Player -> FormFieldState Player e ResourceName
nameField = label "Name" @@= editTextField Players.name NameField (Just 1)

colorField :: PlayerIndex -> Player -> FormFieldState Player e ResourceName
colorField players' = label "Color"
  @@= radioField Players.color (colorOptions players' colorFieldMap)

label :: String -> Widget n -> Widget n
label s w = padBottom (Pad 1) $ vLimit 1 (hLimit 15 $ str s <+> fill ' ') <+> w

colorOptions :: PlayerIndex -> ColorFieldMap -> [(Color, ResourceName, Text)]
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
hasEnoughPlayers screen = minCount <= currentCount
 where
  currentCount = Map.size $ screen ^. players
  minCount     = screen ^. minPlayers

isFull :: RegistrationScreen e -> Bool
isFull screen = maxCount == currentCount
 where
  currentCount = Map.size $ screen ^. players
  maxCount     = screen ^. maxPlayers

chooseCursor
  :: RegistrationScreen e
  -> Maybe
       ([CursorLocation ResourceName] -> Maybe (CursorLocation ResourceName))
chooseCursor screen = case (screen ^. form) of
  Nothing    -> Nothing
  Just form' -> Just (focusRingCursor formFocus $ extractForm form')
