module Main where

import qualified Labyrinth.Players             as Players
import           Labyrinth.Players              ( Player
                                                , Players
                                                )
import qualified Labyrinth.Widgets             as Widgets
import           Labyrinth.Widgets              ( ResourceName )
import           Brick
import qualified Brick.Widgets.Edit            as E
import qualified Brick.Widgets.Border          as B
import qualified Brick.Widgets.Center          as C
import           Brick.Forms                    ( formFocus
                                                , renderForm
                                                , handleFormEvent
                                                , focusedFormInputAttr
                                                , Form
                                                )
import           Brick.Focus                    ( focusRingCursor )
import           Graphics.Vty                   ( Vty )
import qualified Graphics.Vty                  as V
import           Data.Maybe                     ( fromJust )
import           Lens.Micro.TH                  ( makeLenses )
import           Lens.Micro                     ( (^.)
                                                , (&)
                                                , (.~)
                                                )
import           Control.Monad                  ( void )

type PlayerForm e = Form Player e ResourceName

data RegistrationScreen e = RegistrationScreen
  { _playerForm :: PlayerForm e }
makeLenses ''RegistrationScreen

data State e = Splash | Registration (RegistrationScreen e) Players

data Store e = Store
  { _state :: State e
  }
makeLenses ''Store

main :: IO ()
main = void $ customMain buildVty Nothing app (Store Splash)

app :: App (Store e) e ResourceName
app = App { appDraw         = draw
          , appChooseCursor = chooseCursor
          , appHandleEvent  = handleEvent
          , appStartEvent   = return
          , appAttrMap      = attributeMap
          }

draw :: Store e -> [Widget ResourceName]
draw s = case s ^. state of
  Splash                      -> splashScreen
  Registration screen players -> registrationScreen screen players

splashScreen :: [Widget ResourceName]
splashScreen = [str "Welcome and instructions on how to create a game"]

registrationScreen :: RegistrationScreen s -> Players -> [Widget ResourceName]
registrationScreen screen _ = [C.vCenter $ C.hCenter form <=> C.hCenter help]
 where
  form =
    B.border $ padTop (Pad 1) $ hLimit 50 $ renderForm (screen ^. playerForm)
  help = padTop (Pad 1) $ B.borderWithLabel (str "Help") body
  body = str "Foo bar help"

handleEvent
  :: Store e
  -> BrickEvent ResourceName e
  -> EventM ResourceName (Next (Store e))
handleEvent s ev = case s ^. state of
  Splash                      -> handleSplashEvent s ev
  Registration screen players -> handleRegistrationEvent screen players ev

handleSplashEvent
  :: Store e
  -> BrickEvent ResourceName e
  -> EventM ResourceName (Next (Store e))
handleSplashEvent s ev = case ev of
  VtyEvent (V.EvKey V.KEsc   []) -> halt s
  VtyEvent (V.EvKey V.KEnter []) -> continue toRegistration
  _                              -> continue s

handleRegistrationEvent
  :: RegistrationScreen e
  -> Players
  -> BrickEvent ResourceName e
  -> EventM ResourceName (Next (Store e))
handleRegistrationEvent screen players ev = case ev of
  VtyEvent (V.EvKey V.KEsc []) -> halt $ Store $ Registration screen players
  _                            -> do
    playerForm' <- handleFormEvent ev (screen ^. playerForm)
    continue $ Store $ Registration (screen & playerForm .~ playerForm') players

toRegistration :: Store s
toRegistration = Store $ Registration screen players
 where
  players = Players.empty
  options = fromJust $ Widgets.playerFormOptions players
  form    = Widgets.playerForm options
  screen  = RegistrationScreen form

attributeMap :: s -> AttrMap
attributeMap = const $ attrMap
  V.defAttr
  [ (E.editAttr          , V.white `on` V.black)
  , (E.editFocusedAttr   , V.black `on` V.yellow)
  , (focusedFormInputAttr, V.black `on` V.yellow)
  ]

buildVty :: IO Vty
buildVty = do
  v <- V.mkVty =<< V.standardIOConfig
  V.setMode (V.outputIface v) V.Mouse True
  return v

chooseCursor
  :: Store e
  -> [CursorLocation ResourceName]
  -> Maybe (CursorLocation ResourceName)
chooseCursor s = case s ^. state of
  Registration screen _ -> focusRingCursor formFocus (screen ^. playerForm)
  _                     -> neverShowCursor s
