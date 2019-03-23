{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Rank2Types        #-}

module Controller
    ( routeEvent
    , runAndDone
    ) where

import qualified Data.ByteString as BS
import qualified Model.Types     as T
import qualified Brick           as B
import Model.Compiler                   ( runProgram    )
import Model.Parser                     ( parse         )

type EventHandler = forall e. B.BrickEvent () e
                              -> B.EventM () (B.Next T.EtDefugger)

routeEvent :: T.EtDefugger -> EventHandler
routeEvent (Left err) _ = B.halt . Left $ err
routeEvent (Right st) e = case T.mode st of
                               T.Debugger   -> routeDebugEvent st e
                               T.RunAndDone -> B.halt . runAndDone $ st

runAndDone :: T.Defugger -> T.EtDefugger
runAndDone st = do
    p <- parse ( T.dictionary st ) ( T.script st )
    c <- runProgram ( T.computer st ) p
    pure $ st { T.computer = c }

routeDebugEvent :: T.Defugger -> EventHandler
routeDebugEvent st = B.resizeOrQuit ( Right st )
