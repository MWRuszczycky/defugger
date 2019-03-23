{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Rank2Types        #-}

module Controller
    ( routeEvent
    ) where

import qualified Data.ByteString as BS
import qualified Model.Types     as T
import qualified Brick           as B

type EventHandler = forall e. B.BrickEvent () e
                              -> B.EventM () (B.Next T.Debugger)

routeEvent :: T.Debugger -> EventHandler
routeEvent db e = B.resizeOrQuit db e
