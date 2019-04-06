{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Rank2Types        #-}

module Controller
    ( routeEvent
    ) where

import qualified Graphics.Vty    as V
import qualified Brick           as B
import qualified Model.Types     as T
import qualified Model.Debugger  as D
import Brick.Widgets.Edit               ( editor
                                        , getEditContents
                                        , handleEditorEvent )

-- =============================================================== --
-- Local helper type synonyms

type DebugEventMonad = B.EventM T.WgtName (B.Next T.Debugger)
type EventHandler    = forall e. B.BrickEvent T.WgtName e -> DebugEventMonad

-- =============================================================== --
-- Event routers

routeEvent :: T.Debugger -> EventHandler
routeEvent db ev = case T.mode db of
                        T.NormalMode  -> routeNrm db ev
                        T.CommandMode -> routeCmd db ev

routeNrm :: T.Debugger -> EventHandler
-- ^Routes events under debugger normal mode.
routeNrm db (B.VtyEvent (V.EvKey V.KEsc [])) = B.halt db
routeNrm db (B.VtyEvent (V.EvKey k ms)     ) = B.continue . keyEv k ms $ db
routeNrm db (B.VtyEvent (V.EvResize w h)   ) = B.continue . D.resize w h $ db
routeNrm db _                                = B.continue db

routeCmd :: T.Debugger -> EventHandler
-- ^Routes events under debugger command mode.
routeCmd db (B.VtyEvent (V.EvKey V.KEsc []))  = returnToNormal db
routeCmd db (B.VtyEvent (V.EvResize w h)   )  = B.continue . D.resize w h $ db
routeCmd db (B.VtyEvent (V.EvKey V.KEnter _)) = getCommand db
routeCmd db (B.VtyEvent ev)                   = manageCommandEntry db ev
routeCmd db _                                 = B.continue db

-- =============================================================== --
-- Event handlers

keyEv :: V.Key -> [V.Modifier] -> T.Debugger -> T.Debugger
keyEv (V.KChar ' ')  _ db = D.stepForward db
keyEv V.KBS          _ db = D.stepBackward db
keyEv V.KRight       _ db = D.moveCursorRight db
keyEv V.KLeft        _ db = D.moveCursorLeft db
keyEv V.KUp          _ db = D.moveCursorUp db
keyEv V.KDown        _ db = D.moveCursorDown db
keyEv (V.KChar ':' ) _ db = db { T.mode = T.CommandMode }
keyEv (V.KChar 'd' ) _ db = db { T.outFormat = T.Dec, T.inFormat = T.Dec     }
keyEv (V.KChar 'h' ) _ db = db { T.outFormat = T.Hex, T.inFormat = T.Hex     }
keyEv (V.KChar 'a' ) _ db = db { T.outFormat = T.Asc, T.inFormat = T.Asc     }
keyEv (V.KChar '\t') _ db = db { T.wgtFocus = D.nextWidget . T.wgtFocus $ db }
keyEv _              _ db = db

manageCommandEntry :: T.Debugger -> V.Event -> DebugEventMonad
manageCommandEntry db ev = do
    updatedEditor <- handleEditorEvent ev (T.commandEdit db)
    B.continue $ db { T.commandEdit = updatedEditor }

returnToNormal :: T.Debugger -> DebugEventMonad
returnToNormal db = B.continue $
    db { T.message     = ""
       , T.commandEdit = editor T.CommandWgt (Just 1) ""
       , T.mode        = T.NormalMode
       }

getCommand :: T.Debugger -> DebugEventMonad
getCommand db = B.continue $
    db { T.message     = unlines . getEditContents . T.commandEdit $ db
       , T.commandEdit = editor T.CommandWgt (Just 1) ""
       , T.mode        = T.NormalMode
       }
