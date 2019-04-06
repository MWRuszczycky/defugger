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
routeEvent db (B.VtyEvent (V.EvResize w h)   ) = B.continue . resizeEv w h $ db
routeEvent db _                                = B.continue db

keyEv :: V.Key -> [V.Modifier] -> T.Debugger -> T.Debugger
keyEv (V.KChar ' ')  _ db = D.stepForward db
keyEv V.KBS          _ db = D.stepBackward db
keyEv V.KRight       _ db = D.moveCursorRight db
keyEv V.KLeft        _ db = D.moveCursorLeft db
keyEv V.KUp          _ db = D.moveCursorUp db
keyEv V.KDown        _ db = D.moveCursorDown db
keyEv (V.KChar 'd' ) _ db = db { T.outFormat = T.Dec, T.inFormat = T.Dec    }
keyEv (V.KChar 'h' ) _ db = db { T.outFormat = T.Hex, T.inFormat = T.Hex    }
keyEv (V.KChar 'a' ) _ db = db { T.outFormat = T.Asc, T.inFormat = T.Asc    }
keyEv (V.KChar '\t') _ db = db { T.wgtFocus = changeFocus . T.wgtFocus $ db }
keyEv _              _ db = db

resizeEv :: Int -> Int -> T.Debugger -> T.Debugger
resizeEv w h db = let pv        = T.progView db
                      mv        = T.memView db
                      progRow   = D.getPositionRow db
                      memRow    = D.getAddress db
                      wgtHeight = h - 3
                  in  db { T.termWidth  = w
                         , T.termHeight = h
                         , T.progView   = resizeWgtView wgtHeight progRow pv
                         , T.memView    = resizeWgtView wgtHeight memRow  mv
                         }

resizeWgtView :: Int -> Int -> (Int, Int) -> (Int, Int)
resizeWgtView h n (n0,n1)
    | h == oldHeight              = (n0, n1)
    | h > oldHeight               = (n0', n0' + h - 1)
    | h < oldHeight && n < n0 + h = (n0, n0 + h - 1)
    | otherwise                   = (n - h + 1, n)
    where oldHeight = n1 - n0 + 1
          n0'       = min 0 (n1 - h + 1)

---------------------------------------------------------------------
-- Helpers

changeFocus :: T.WgtName -> T.WgtName
changeFocus T.ProgramWgt = T.MemoryWgt
changeFocus T.MemoryWgt  = T.OutputWgt
changeFocus T.OutputWgt  = T.InputWgt
changeFocus T.InputWgt   = T.ProgramWgt
changeFocus T.StatusWgt  = T.ProgramWgt
