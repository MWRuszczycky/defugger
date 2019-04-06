{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Rank2Types        #-}

module Controller
    ( routeEvent
    ) where

import qualified Graphics.Vty    as V
import qualified Brick           as B
import qualified Model.Types     as T
import qualified Model.Debugger  as D

type DebugEventMonad = B.EventM T.WgtName (B.Next T.Debugger)
type EventHandler    = forall e. B.BrickEvent T.WgtName e -> DebugEventMonad

routeEvent :: T.Debugger -> EventHandler
routeEvent db (B.VtyEvent (V.EvKey V.KEsc [])) = B.halt db
routeEvent db (B.VtyEvent (V.EvKey k ms)     ) = B.continue . keyEv k ms $ db
routeEvent db (B.VtyEvent (V.EvResize w h)   ) = B.continue . D.resize w h $ db
routeEvent db _                                = B.continue db

keyEv :: V.Key -> [V.Modifier] -> T.Debugger -> T.Debugger
keyEv (V.KChar ' ')  _ db = D.stepForward db
keyEv V.KBS          _ db = D.stepBackward db
keyEv V.KRight       _ db = D.moveCursorRight db
keyEv V.KLeft        _ db = D.moveCursorLeft db
keyEv V.KUp          _ db = D.moveCursorUp db
keyEv V.KDown        _ db = D.moveCursorDown db
keyEv (V.KChar 'd' ) _ db = db { T.outFormat = T.Dec, T.inFormat = T.Dec     }
keyEv (V.KChar 'h' ) _ db = db { T.outFormat = T.Hex, T.inFormat = T.Hex     }
keyEv (V.KChar 'a' ) _ db = db { T.outFormat = T.Asc, T.inFormat = T.Asc     }
keyEv (V.KChar '\t') _ db = db { T.wgtFocus = D.nextWidget . T.wgtFocus $ db }
keyEv _              _ db = db
