module Labyrinth.UI.Screen.Setup
  ( SetupS
  , submitPlayer
  , validate
  , editPlayer
  , playerAt
  , processForm
  , chooseCursor
  , extractPlayer
  , initial
  , draw
  , players
  --
  , form
  , register
  , hasEnoughPlayers
  )
where

import           Brick
import           Brick.Focus                                                  ( focusRingCursor )
import           Brick.Forms                                                  ( Form
                                                                              , FormFieldState
                                                                              , newForm
                                                                              , radioField
                                                                              , editTextField
                                                                              , renderForm
                                                                              , formState
                                                                              , formFocus
                                                                              , (@@=)
                                                                              )
import           Control.Lens                                                 ( makeLenses
                                                                              , (^.)
                                                                              , (?~)
                                                                              , (&)
                                                                              )
import           Data.Text                                                    ( Text )
import qualified Data.Text                     as Text
import qualified Data.Map.Strict               as Map
import           Data.Map.Strict                                              ( (!) )
import           Control.Monad                                                ( guard )
import           Labyrinth.Game                                               ( Player(..)
                                                                              , Players
                                                                              , Color(..)
                                                                              , Configuration
                                                                              , PlayOrder(..)
                                                                              )
import qualified Labyrinth.Game.Configuration  as C
import qualified Labyrinth.Game.Player         as P
import           Labyrinth.UI.Widget
import           Labyrinth.UI.Internal

type PlayerForm e = Form Player e Name
type FormProcessor e = PlayerForm e -> EventM Name (PlayerForm e)
data TheForm e = AddPlayerForm (PlayerForm e) | EditPlayerForm (PlayerForm e)

instance Show (TheForm e) where
  show (AddPlayerForm  _) = "AddPlayerForm"
  show (EditPlayerForm _) = "EditPlayerForm"

data SetupS e = SetupS
  { _form :: Maybe (TheForm e)  -- _form is the user interface
  , _conf :: Configuration      -- _conf is just an environment
  } deriving (Show)
makeLenses ''SetupS

initial :: SetupS e
initial = SetupS { _form = mkAddPlayerForm C.initial, _conf = C.initial }

draw :: SetupS s -> Widget Name
draw s = theForm <=> registered <=> help

 where
  theForm = case s ^. form of
    Just (AddPlayerForm  form') -> titleBox " Add Player " $ renderForm form'
    Just (EditPlayerForm form') -> titleBox " Edit Player " $ renderForm form'
    _                           -> emptyWidget

  registered = case C.toList $ s ^. conf of
    [] -> emptyWidget
    ps -> titleBox " Players " $ vBox $ map toPlayer ps

  toPlayer p = playerLabel 35 p <+> editPlayerCommand p

  help          = submitCommand <=> beginCommand <=> quitCommand

  submitCommand = if validate s then txt "Enter: add player" else emptyWidget

  beginCommand  = if hasEnoughPlayers s then txt "Ctrl+p: begin game" else emptyWidget

  editPlayerCommand p =
    str $ " " <> "Edit: Ctrl+" <> ["a", "s", "d", "f"] !! fromEnum (p ^. P.order)

  quitCommand = txt "Ctrl+q: quit"

submitPlayer :: SetupS e -> SetupS e
submitPlayer s = maybe s (register s) (extractPlayer s)

validate :: SetupS e -> Bool
validate s = maybe False (val . extractForm) (s ^. form)
  where val = (0 <) . Text.length . (^. P.name) . formState

editPlayer :: SetupS e -> Player -> SetupS e
editPlayer s player = s & form ?~ mkEditPlayerForm (s ^. conf) player

playerAt :: SetupS e -> PlayOrder -> Maybe Player
playerAt s = C.playerAt (s ^. conf)

processForm :: SetupS e -> FormProcessor e -> EventM Name (SetupS e)
processForm s process = case s ^. form of
  Just (AddPlayerForm  form') -> processAndWrap AddPlayerForm form'
  Just (EditPlayerForm form') -> processAndWrap EditPlayerForm form'
  Nothing                     -> return s
 where
  processAndWrap wrap f = do
    f' <- process f
    return $ s & form ?~ wrap f'

extractPlayer :: SetupS e -> Maybe Player
extractPlayer s = formState . extractForm <$> (s ^. form)

extractForm :: TheForm e -> PlayerForm e
extractForm (AddPlayerForm  f) = f
extractForm (EditPlayerForm f) = f

register :: SetupS e -> Player -> SetupS e
register s player = SetupS form' conf'
 where
  conf' = C.insert player (s ^. conf)
  form' = mkAddPlayerForm conf'

mkAddPlayerForm :: Configuration -> Maybe (TheForm e)
mkAddPlayerForm cfg = case nextDefaultPlayer cfg of
  Just player -> Just $ AddPlayerForm (mkForm cfg player)
  Nothing     -> Nothing

mkEditPlayerForm :: Configuration -> Player -> TheForm e
mkEditPlayerForm cfg p = EditPlayerForm (mkForm (C.delete p cfg) p)

nextDefaultPlayer :: Configuration -> Maybe Player
nextDefaultPlayer cfg = case C.availableColors cfg of
  []      -> Nothing
  colors' -> Just (Player "" (head colors') (toEnum $ length P.colors - length colors'))

mkForm :: Configuration -> Player -> PlayerForm e
mkForm cfg = newForm [nameField, colorField cfg]

nameField :: Player -> FormFieldState Player e Name
nameField = label "Name" @@= editTextField P.name NameField (Just 1)

colorField :: Configuration -> Player -> FormFieldState Player e Name
colorField cfg = label "Color" @@= radioField P.color (colorOptions cfg)

colorOptions :: Configuration -> [(Color, Name, Text)]
colorOptions cfg = zip3 colors fields labels
 where
  colors = C.availableColors cfg
  labels = map (Text.pack . show) colors
  fields = map (fieldByColor !) colors
  fieldByColor =
    Map.fromList [(Yellow, YellowField), (Red, RedField), (Blue, BlueField), (Green, GreenField)]

hasEnoughPlayers :: SetupS e -> Bool
hasEnoughPlayers = C.hasEnoughPlayers . (^. conf)

chooseCursor :: SetupS e -> Maybe ([CursorLocation Name] -> Maybe (CursorLocation Name))
chooseCursor s = case (s ^. form) of
  Nothing    -> Nothing
  Just form' -> Just (focusRingCursor formFocus $ extractForm form')

players :: SetupS e -> Maybe Players
players s = do
  guard (hasEnoughPlayers s)
  Just $ s ^. (conf . C.players)
