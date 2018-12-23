module Main where

import           Labyrinth.Players              ( Player(..)
                                                , Color(..)
                                                )
import qualified Labyrinth.Players             as Player
import qualified Labyrinth.Widgets             as Widgets
import           Labyrinth.Widgets              ( PlayerForm
                                                , ResourceName
                                                )
import           Brick
import           Brick.Forms                    ( formFocus
                                                , renderForm
                                                )
import           Brick.Focus                    ( focusRingCursor )
import qualified Graphics.Vty                  as Vty

main :: IO ()
main = do
  let buildVty = do
        v <- Vty.mkVty =<< Vty.standardIOConfig
        Vty.setMode (Vty.outputIface v) Vty.Mouse True
        return v
      options = Widgets.playerFormOptions Player.empty
      player  = Player "" Yellow
      f       = Widgets.playerForm options player
  _ <- customMain buildVty Nothing app f
  putStrLn "done"

draw :: PlayerForm e -> [Widget ResourceName]
draw f = [renderForm f]

app :: App (PlayerForm e) e ResourceName
app = App
  { appDraw         = draw
  , appHandleEvent  = (\s ev -> case ev of
                        VtyEvent (Vty.EvKey Vty.KEsc []) -> halt s
                        _ -> continue s
                      )
  , appChooseCursor = focusRingCursor formFocus
  , appStartEvent   = return
  , appAttrMap      = const $ attrMap Vty.defAttr []
  }
