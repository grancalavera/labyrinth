module Main where

import qualified Labyrinth.Players             as Players
import           Labyrinth.Players              ( Player )
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


type PlayerForm e = Form Player e ResourceName

data Store e = Store
  { _playerForm :: PlayerForm e
  }
makeLenses ''Store

main :: IO ()
main = do
  let options = fromJust $ Widgets.playerFormOptions Players.empty
      store   = Store $ Widgets.playerForm options
  _ <- customMain buildVty Nothing app store
  putStrLn "done"

app :: App (Store e) e ResourceName
app = App { appDraw         = draw
          , appChooseCursor = chooseCursor
          , appHandleEvent  = handleEvent
          , appStartEvent   = return
          , appAttrMap      = attributeMap
          }

draw :: Store e -> [Widget ResourceName]
draw store = [C.vCenter $ C.hCenter form <=> C.hCenter help]
 where
  form =
    B.border $ padTop (Pad 1) $ hLimit 50 $ renderForm (store ^. playerForm)
  help = padTop (Pad 1) $ B.borderWithLabel (str "Help") body
  body = str "Foo bar help"

handleEvent
  :: Store e
  -> BrickEvent ResourceName e
  -> EventM ResourceName (Next (Store e))
handleEvent s ev = case ev of
  VtyEvent (V.EvKey V.KEsc []) -> halt s
  _                            -> do
    playerForm' <- handleFormEvent ev (s ^. playerForm)
    continue (s & playerForm .~ playerForm')

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

-- in the future we'll need to check the state of the
-- application and choose how to handle focus, right
-- now this works as POC only.
chooseCursor
  :: Store e
  -> [CursorLocation ResourceName]
  -> Maybe (CursorLocation ResourceName)
chooseCursor = focusRingCursor formFocus . (^. playerForm)
