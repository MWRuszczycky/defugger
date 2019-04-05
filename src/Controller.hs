{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Rank2Types        #-}

module Controller
    ( routeEvent
    ) where

import qualified Data.Vector     as Vec
import qualified Graphics.Vty    as V
import qualified Brick           as B
import qualified Model.Types     as T
import qualified Model.Compiler  as C

type EventHandler = forall e. B.BrickEvent T.WgtName e
                              -> B.EventM T.WgtName (B.Next T.Debugger)

routeEvent :: T.Debugger -> EventHandler
routeEvent db (B.VtyEvent (V.EvKey V.KEsc [])) = B.halt db
routeEvent db (B.VtyEvent (V.EvKey k ms)     ) = B.continue . keyEv k ms $ db
routeEvent db (B.VtyEvent (V.EvResize w h)   ) = B.continue . resizeEv w h $ db
routeEvent db e                                = B.resizeOrQuit db e

keyEv :: V.Key -> [V.Modifier] -> T.Debugger -> T.Debugger
keyEv (V.KChar ' ')  _ db = executeStatement db
keyEv V.KBS          _ db = revertStatement db
keyEv V.KRight       _ db = moveCursorRight db
keyEv V.KLeft        _ db = moveCursorLeft db
keyEv V.KUp          _ db = moveCursorUp db
keyEv V.KDown        _ db = moveCursorDown db
keyEv (V.KChar 'd' ) _ db = db { T.outFormat = T.Dec, T.inFormat = T.Dec    }
keyEv (V.KChar 'h' ) _ db = db { T.outFormat = T.Hex, T.inFormat = T.Hex    }
keyEv (V.KChar 'a' ) _ db = db { T.outFormat = T.Asc, T.inFormat = T.Asc    }
keyEv (V.KChar '\t') _ db = db { T.wgtFocus = changeFocus . T.wgtFocus $ db }
keyEv _              _ db = db

executeStatement :: T.Debugger -> T.Debugger
executeStatement db = either (const db) go . C.stepForward $ db
    where go db' = let newDB = shiftBoth db'
                   in  newDB { T.cursor = C.getPosition newDB }

revertStatement :: T.Debugger -> T.Debugger
revertStatement db = either (const db) go . C.stepBackward $ db
    where go db' = let newDB = shiftBoth db'
                   in  newDB { T.cursor = C.getPosition newDB }

moveCursorRight :: T.Debugger -> T.Debugger
moveCursorRight db
    | atEnd     = db
    | otherwise = db { T.cursor = x + 1 }
    where x     = T.cursor db
          atEnd = (== x) . subtract 1 . Vec.length . T.program $ db

moveCursorLeft :: T.Debugger -> T.Debugger
moveCursorLeft db
    | x == 0    = db
    | otherwise = db { T.cursor = x - 1 }
    where x = T.cursor db

moveCursorUp :: T.Debugger -> T.Debugger
moveCursorUp db
    | x < 0     = db
    | otherwise = db { T.cursor = x }
    where x = T.cursor db - T.progWidth db

moveCursorDown :: T.Debugger -> T.Debugger
moveCursorDown db
    | atEnd     = db
    | otherwise = db { T.cursor = y }
    where y     = T.cursor db + T.progWidth db
          atEnd = (< y) . (subtract 1) . Vec.length . T.program $ db

resizeEv :: Int -> Int -> T.Debugger -> T.Debugger
resizeEv w h db = let pv        = T.progView db
                      mv        = T.memView db
                      progRow   = getProgRow db
                      memRow    = C.getAddress db
                      wgtHeight = h - 3
                  in  db { T.termWidth  = w
                         , T.termHeight = h
                         , T.progView   = resizeWgtView wgtHeight progRow pv
                         , T.memView    = resizeWgtView wgtHeight memRow  mv
                         }

---------------------------------------------------------------------
-- Helpers

shiftBoth :: T.Debugger -> T.Debugger
shiftBoth = shiftMem . shiftProg

shiftProg :: T.Debugger -> T.Debugger
shiftProg db = let oldView = T.progView db
                   progRow = getProgRow db
               in  db { T.progView = shiftPos oldView progRow }

shiftMem :: T.Debugger -> T.Debugger
shiftMem db = let oldView    = T.memView db
                  memAddress = C.getAddress db
              in  db { T.memView = shiftPos oldView memAddress }

getProgRow :: T.Debugger -> Int
getProgRow db = quot (C.getPosition db) (T.progWidth db)

shiftPos :: (Int, Int) -> Int -> (Int, Int)
shiftPos (n0,n1) n
    | n < n0    = (n, n + n1 - n0)
    | n > n1    = (n - (n1 - n0), n)
    | otherwise = (n0,n1)

resizeWgtView :: Int -> Int -> (Int, Int) -> (Int, Int)
resizeWgtView h n (n0,n1)
    | h == oldHeight              = (n0, n1)
    | h > oldHeight               = (n0', n0' + h - 1)
    | h < oldHeight && n < n0 + h = (n0, n0 + h - 1)
    | otherwise                   = (n - h + 1, n)
    where oldHeight = n1 - n0 + 1
          n0'       = min 0 (n1 - h + 1)

changeFocus :: T.WgtName -> T.WgtName
changeFocus T.ProgramWgt = T.MemoryWgt
changeFocus T.MemoryWgt  = T.OutputWgt
changeFocus T.OutputWgt  = T.InputWgt
changeFocus T.InputWgt   = T.ProgramWgt
changeFocus T.StatusWgt  = T.ProgramWgt
