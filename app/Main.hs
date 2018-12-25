module Main where

import qualified Labyrinth.Players             as Players
import qualified Labyrinth.Widgets             as Widgets
import           Labyrinth.Widgets              ( PlayerForm
                                                , ResourceName
                                                )
import           Brick
import qualified Brick.Widgets.Edit            as E
import qualified Brick.Widgets.Border          as B
import qualified Brick.Widgets.Center          as C
import           Brick.Forms                    ( formFocus
                                                , renderForm
                                                , handleFormEvent
                                                , focusedFormInputAttr
                                                )
import           Brick.Focus                    ( focusRingCursor )
import qualified Graphics.Vty                  as V
import           Data.Maybe                     ( fromJust )

main :: IO ()
main = do
  let buildVty = do
        v <- V.mkVty =<< V.standardIOConfig
        V.setMode (V.outputIface v) V.Mouse True
        return v
      -- we can only use `fromJust` safely here because we know
      -- that by construction `Players.empty` will yield a
      -- `PlayerFormOptions` with all the `Color` available
      options = fromJust $ Widgets.playerFormOptions Players.empty
      f       = Widgets.playerForm options
  _ <- customMain buildVty Nothing app f
  putStrLn "done"

app :: App (PlayerForm e) e ResourceName
app = App { appDraw         = draw
          , appChooseCursor = focusRingCursor formFocus
          , appHandleEvent  = handleEvent
          , appStartEvent   = return
          , appAttrMap      = attributeMap
          }

draw :: PlayerForm e -> [Widget ResourceName]
draw f = [C.vCenter $ C.hCenter form <=> C.hCenter help]
 where
  form = B.border $ padTop (Pad 1) $ hLimit 50 $ renderForm f
  help = padTop (Pad 1) $ B.borderWithLabel (str "Help") body
  body = str "Foo bar help"

handleEvent
  :: PlayerForm e
  -> BrickEvent ResourceName e
  -> EventM ResourceName (Next (PlayerForm e))
handleEvent s ev = case ev of
  VtyEvent (V.EvKey V.KEsc []) -> halt s
  _                            -> handleFormEvent ev s >>= continue

attributeMap :: s -> AttrMap
attributeMap = const $ attrMap
  V.defAttr
  [ (E.editAttr          , V.white `on` V.black)
  , (E.editFocusedAttr   , V.black `on` V.yellow)
  , (focusedFormInputAttr, V.black `on` V.yellow)
  ]
