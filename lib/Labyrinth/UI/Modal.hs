module Labyrinth.UI.Modal
  ( Modal(..)
  , chooseCursor
  , draw
  , quitConfirmation
  , dialog
  , onTrue
  , onFalse
  )
where

import           Brick
import qualified Brick.Widgets.Center          as C
import qualified Brick.Widgets.Dialog          as D
import           Lens.Micro                     ( (^.) )
import           Lens.Micro.TH                  ( makeLenses )
import           Brick.Widgets.Dialog           ( Dialog(..) )
import           Labyrinth.UI.Internal

type Callback s e = EventM Name (Next (s e))

data Modal s e = Modal
  { _dialog :: Dialog Bool
  , _dialogBody :: Widget Name
  , _onFalse :: Callback s e
  , _onTrue :: Callback s e
  }
makeLenses ''Modal

draw :: Modal s e -> Widget Name
draw modal = D.renderDialog theDialog $ C.hCenter $ padAll 1 theBody
 where
  theDialog = modal ^. dialog
  theBody   = modal ^. dialogBody

chooseCursor
  :: Modal s e -> Maybe ([CursorLocation Name] -> Maybe (CursorLocation Name))
chooseCursor _ = Nothing

quitConfirmation :: Callback s e -> Callback s e -> Modal s e
quitConfirmation whenTrue whenFalse = Modal
  { _dialog     = D.dialog (Just " Labyrinth ") (Just (0, choices)) 50
  , _dialogBody = txt "Do you want to quit Labyrinth?"
  , _onFalse    = whenFalse
  , _onTrue     = whenTrue
  }
  where choices = [("Stay", False), ("Quit", True)]
