{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Rank2Types        #-}

module Controller
    ( routeEvent
    ) where

import qualified Graphics.Vty    as V
import qualified Brick           as B
import qualified Model.Types     as T
import qualified Model.Compiler  as C

type EventHandler = forall e. B.BrickEvent () e
                              -> B.EventM () (B.Next T.Debugger)

routeEvent :: T.Debugger -> EventHandler
routeEvent db (B.VtyEvent (V.EvKey V.KEsc [])) = B.halt db
routeEvent db (B.VtyEvent (V.EvKey k ms)     ) = B.continue . keyEv k ms $ db
routeEvent db (B.VtyEvent (V.EvResize w h)   ) = B.continue . resizeEv w h $ db
routeEvent db e                                = B.resizeOrQuit db e

keyEv :: V.Key -> [V.Modifier] -> T.Debugger -> T.Debugger
keyEv V.KRight _ db      = either (const db) shiftBoth . C.stepForward $ db
keyEv V.KLeft  _ db      = either (const db) shiftBoth . C.stepBackward $ db
keyEv (V.KChar 'd') _ db = db { T.outFormat = T.Dec, T.inFormat = T.Dec }
keyEv (V.KChar 'h') _ db = db { T.outFormat = T.Hex, T.inFormat = T.Hex }
keyEv (V.KChar 'a') _ db = db { T.outFormat = T.Asc, T.inFormat = T.Asc }
keyEv _             _ db = db

resizeEv :: Int -> Int -> T.Debugger -> T.Debugger
resizeEv w h db = db { T.termWidth = w, T.termHeight = h }

---------------------------------------------------------------------
-- Helpers

shiftBoth :: T.Debugger -> T.Debugger
shiftBoth = shiftMem . shiftProg

shiftProg :: T.Debugger -> T.Debugger
shiftProg db = let oldView = T.progView db
                   progRow = quot (C.getPosition db) (T.progWidth db)
               in  db { T.progView = shiftPos oldView progRow }

shiftMem :: T.Debugger -> T.Debugger
shiftMem db = let oldView    = T.memView db
                  memAddress = C.getAddress db
              in  db { T.memView = shiftPos oldView memAddress }

shiftPos :: (Int, Int) -> Int -> (Int, Int)
shiftPos (n0,n1) n
    | n < n0    = (n, n + n1 - n0)
    | n > n1    = (n - (n1 - n0), n)
    | otherwise = (n0,n1)
