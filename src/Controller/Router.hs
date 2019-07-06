{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns      #-}

module Controller.Router
    ( routeEvent
    ) where

-- =============================================================== --
-- Managing user input while running in debugger mode.             --
-- This basically defines the Brick interface for the defugger.    --
-- =============================================================== --

import qualified Graphics.Vty             as V
import qualified Brick                    as B
import qualified Model.Types              as T
import qualified Model.Debugger.Debugger  as D
import Control.Concurrent.Async           as A
import Brick.BChan                              ( writeBChan        )
import Control.Monad.Except                     ( runExceptT
                                                , liftIO            )
import Controller.Commands                      ( parseCommand      )
import Controller.KeyBindings                   ( parseKey          )
import Brick.Widgets.Edit                       ( editor
                                                , getEditContents
                                                , handleEditorEvent )

-- =============================================================== --
-- Local helper type synonyms

type DebugEventMonad = B.EventM T.WgtName (B.Next T.Debugger)
type EventHandler    = B.BrickEvent T.WgtName T.DebugEvent -> DebugEventMonad

-- =============================================================== --
-- Event router -- This is the controller entry point.

routeEvent :: T.Debugger -> EventHandler
routeEvent db ev = case T.mode db of
                        T.CommandMode      -> routeCommandEvent db ev
                        T.ProcessingMode c -> routeProcessingEvent c db ev
                        _                  -> routeGeneral db ev

-- =============================================================== --
-- General event routing

---------------------------------------------------------------------
-- Router

routeGeneral :: T.Debugger -> EventHandler
routeGeneral db (B.VtyEvent (V.EvKey k _))     = keyEvent k db
routeGeneral db (B.VtyEvent (V.EvResize w h) ) = B.continue . D.resize w h $ db
routeGeneral db _                              = B.continue db

---------------------------------------------------------------------
-- Key events

keyEvent :: V.Key -> T.Debugger -> DebugEventMonad
keyEvent k db =
    case parseKey k (T.mode db) (T.wgtFocus db) of
         T.PureCmd f      -> B.continue . f $ db
         T.QuitCmd        -> B.halt db
         T.ErrorCmd e     -> B.continue $ db { T.message = e }
         T.SimpleIOCmd _  -> B.continue db
         T.ComplexIOCmd _ -> B.continue db
         T.TandemCmd f    -> isolate f db
         T.HScrollCmd w n -> B.hScrollBy (B.viewportScroll w) n >> B.continue db
         T.VScrollCmd w n -> B.vScrollBy (B.viewportScroll w) n >> B.continue db

-- =============================================================== --
-- Events in command mode

---------------------------------------------------------------------
-- Router

routeCommandEvent :: T.Debugger -> EventHandler
routeCommandEvent db (B.VtyEvent (V.EvKey V.KEsc _ )) =
    abortToNormalMode db

routeCommandEvent db (B.VtyEvent (V.EvResize w h) ) =
    B.continue . D.resize w h $ db

routeCommandEvent db (B.VtyEvent (V.EvKey V.KEnter _ )) =
    handleCommand db

routeCommandEvent db (B.VtyEvent ev) =
    manageCommandEntry db ev

routeCommandEvent db _ =
    B.continue db

---------------------------------------------------------------------
-- Command mode event handlers

manageCommandEntry :: T.Debugger -> V.Event -> DebugEventMonad
-- ^Hand off Vty events for editing the contents of the command line
-- to the Brick runtime system.
manageCommandEntry db ev = do
    updatedEditor <- handleEditorEvent ev (T.commandEdit db)
    B.continue $ db { T.commandEdit = updatedEditor }

abortToNormalMode :: T.Debugger -> DebugEventMonad
-- ^Abort entering a command and return to normal mode.
abortToNormalMode db = B.continue $
    db { T.message     = ""
       , T.commandEdit = editor T.CommandWgt (Just 1) ""
       , T.mode        = T.NormalMode
       }

handleCommand :: T.Debugger -> DebugEventMonad
-- ^Read the command entered and execute it.
handleCommand db0 =
    let db1        = db0 { T.commandEdit = editor T.CommandWgt (Just 1) ""
                         , T.mode        = T.NormalMode }
        cmdStr     = getEditContents . T.commandEdit $ db0
        goIO f db  = runExceptT (f db) >>= pure . either (err db) id
        err db msg = db { T.message = msg }
    in  case parseCommand . words . unlines $ cmdStr of
             T.PureCmd f      -> B.continue . f $ db1
             T.SimpleIOCmd f  -> liftIO ( goIO f db1 ) >>= B.continue
             T.ComplexIOCmd f -> B.suspendAndResume $ goIO f db1
             T.ErrorCmd e     -> B.continue $ db1 { T.message = e }
             T.QuitCmd        -> B.halt db1
             _                -> B.continue db1

-- =============================================================== --
-- Events when processing a computation on the debugger

routeProcessingEvent :: Async T.Debugger -> T.Debugger -> EventHandler
routeProcessingEvent c db (B.VtyEvent (V.EvKey V.KEsc _ )) = do
    liftIO $ A.cancel c
    B.continue $ db { T.mode    = T.NormalMode
                    , T.message = "Jump aborted" }

routeProcessingEvent c _ (B.AppEvent T.ComputationDone) =
    liftIO ( A.wait c ) >>= B.continue

routeProcessingEvent _ db _ =
    B.continue db

---------------------------------------------------------------------
-- Isolating computations on the debugger in their own thread

isolate :: (T.Debugger -> T.Debugger) -> T.Debugger -> DebugEventMonad
isolate go db = do
    let jumpMessage = "Esc to abort jump..."
    c <- liftIO . A.async $ do let !db' = go db
                               writeBChan (T.channel db) T.ComputationDone
                               if T.message db' == jumpMessage
                                  then pure . D.noMessage $ db'
                                  else pure db'
    B.continue $ db { T.mode    = T.ProcessingMode c
                    , T.message = jumpMessage
                    }
