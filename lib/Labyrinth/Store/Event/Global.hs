module Labyrinth.Store.Event.Global
  ( handle
  , isGlobalEvent
  )
where

import           Brick
import qualified Graphics.Vty                  as V
import qualified Data.Map.Strict               as Map
import           Data.Map.Strict                ( Map
                                                , (!?)
                                                )
import           Data.Maybe                     ( fromMaybe )
import           Labyrinth.UI                   ( Name )
import           Labyrinth.Store.Internal

type GlobalEventHandler e
  = Store e -> BrickEvent Name e -> EventM Name (Next (Store e))

isGlobalEvent :: Ord e => BrickEvent Name e -> Bool
isGlobalEvent = (`elem` Map.keys eventMap)

handle :: Ord e => GlobalEventHandler e
handle store ev = (fromMaybe (const . continue) $ eventMap !? ev) store ev

promptToQuit :: GlobalEventHandler e
promptToQuit store _ = halt store

eventMap :: Ord e => Map (BrickEvent Name e) (GlobalEventHandler e)
eventMap =
  Map.fromList [(VtyEvent (V.EvKey (V.KChar 'q') [V.MCtrl]), promptToQuit)]
