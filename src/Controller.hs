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
routeEvent db (B.VtyEvent (V.EvKey k ms))      = B.continue . keyEv k ms $ db
routeEvent db e                                = B.resizeOrQuit db e

keyEv :: V.Key -> [V.Modifier] -> T.Debugger -> T.Debugger
keyEv V.KRight _ db = stepForward db
keyEv V.KLeft  _ db = db
keyEv _        _ db = db

stepForward :: T.Debugger -> T.Debugger
stepForward db =
    let n = T.position db
    in  case T.program db !! n of
             T.DBEnd         -> db
             T.DBStart       -> db { T.position = n + 1 }
             T.DBIncrement   -> stepFwdComputer (n+1) C.increment db
             T.DBDecrement   -> stepFwdComputer (n+1) C.decrement db
             T.DBAdvance     -> stepFwdComputer (n+1) C.advance   db
             T.DBBackup      -> stepFwdComputer (n+1) C.backup    db
             T.DBReadIn      -> stepFwdComputer (n+1) C.readIn    db
             T.DBWriteOut    -> stepFwdComputer (n+1) C.writeOut  db
             T.DBCloseLoop y -> db { T.position = y }
             T.DBOpenLoop  y -> let x = T.focus . T.memory . T.computer $ db
                                in  if x == 0
                                       then db { T.position = y+1 }
                                       else db { T.position = n+1 }

stepFwdComputer :: Int -> T.Computation -> T.Debugger -> T.Debugger
stepFwdComputer n f db = either (const db) go . f . T.computer $ db
    where go c = db { T.position = n, T.computer = c }
