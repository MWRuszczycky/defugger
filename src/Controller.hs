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
keyEv V.KRight _ db      = either (const db) id . C.stepForward $ db
keyEv V.KLeft  _ db      = either (const db) id . C.stepBackward $ db
keyEv (V.KChar 'd') _ db = db { T.outFormat = T.Dec, T.inFormat = T.Dec }
keyEv (V.KChar 'h') _ db = db { T.outFormat = T.Hex, T.inFormat = T.Hex }
keyEv (V.KChar 'a') _ db = db { T.outFormat = T.Asc, T.inFormat = T.Asc }
keyEv _             _ db = db

resizeEv :: Int -> Int -> T.Debugger -> T.Debugger
resizeEv w h db = db { T.termWidth = w, T.termHeight = h }
