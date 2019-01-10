module Labyrinth.UI.Modal
  ( Modal(..)
  , ModalCallback
  , chooseCursor
  , draw
  , dialog
  , onTrue
  , onFalse
  , mkModal
  )
where

import           Brick
import qualified Brick.Widgets.Center          as C
import qualified Brick.Widgets.Dialog          as D
import           Lens.Micro                     ( (^.) )
import           Lens.Micro.TH                  ( makeLenses )
import           Brick.Widgets.Dialog           ( Dialog(..) )
import           Labyrinth.UI.Internal

type ModalCallback s e = EventM Name (Next (s e))
type ModalOptions = (Int, [(String, Bool)])

data Modal s e = Modal
  { _dialog :: Dialog Bool
  , _dialogBody :: Widget Name
  , _onTrue :: ModalCallback s e
  , _onFalse :: ModalCallback s e
  , _description :: String
  }
makeLenses ''Modal

instance Show (Modal s e) where
  show m = "Modal: " <> m ^. description

draw :: Modal s e -> Widget Name
draw modal = D.renderDialog dialog' $ C.hCenter $ padAll 1 body'
 where
  dialog' = modal ^. dialog
  body'   = modal ^. dialogBody

chooseCursor
  :: Modal s e -> Maybe ([CursorLocation Name] -> Maybe (CursorLocation Name))
chooseCursor _ = Nothing

mkModal
  :: String
  -> Widget Name
  -> ModalOptions
  -> ModalCallback s e
  -> ModalCallback s e
  -> Modal s e
mkModal desc body options onT onF = Modal
  { _dialog      = D.dialog (Just " Labyrinth ") (Just options) 50
  , _dialogBody  = body
  , _onTrue      = onT
  , _onFalse     = onF
  , _description = desc
  }
