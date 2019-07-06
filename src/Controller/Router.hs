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
import Controller.CommandBindings               ( parseCommand      )
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
-- Event Routers
-- Most events can be handled using key-bindings to map key events to
-- commands for execution. The general router (routeGeneral) handles
-- these events as well as resize events. This applies to Normal Mode
-- and Help Mode. In contrast, Command Mode and Processing Mode need
-- to be handeled separately. Command Mode relies on a Brick edit
-- widget for command input, so we want to let it deal with most
-- events before getting the user input and translating this to a
-- command for execution. Processing Mode is used when a potentially
-- long or non-halting is executing and we need to run it in its own
-- thread so it can be aborted if necessary. In this case most events
-- are simply ignored.

---------------------------------------------------------------------
-- General event router for Normal and Help Mode

routeGeneral :: T.Debugger -> EventHandler
routeGeneral db (B.VtyEvent (V.EvKey k _))     = keyEvent k db
routeGeneral db (B.VtyEvent (V.EvResize w h) ) = B.continue . D.resize w h $ db
routeGeneral db _                              = B.continue db

---------------------------------------------------------------------
-- Events in Command Mode

routeCommandEvent :: T.Debugger -> EventHandler
-- |The <esc> key aborts entry of commands and Command Mode.
routeCommandEvent db (B.VtyEvent (V.EvKey V.KEsc _ )) =
    B.continue $ db { T.message     = ""
                    , T.commandEdit = editor T.CommandWgt (Just 1) ""
                    , T.mode        = T.NormalMode
                    }

-- |Handle resizing of the debugger terminal window.
routeCommandEvent db (B.VtyEvent (V.EvResize w h) ) =
    B.continue . D.resize w h $ db

-- |Handle the command when the user presses the <enter> key. This
-- returns the debugger to Normal Mode.
routeCommandEvent db (B.VtyEvent (V.EvKey V.KEnter _ )) =
    commandEvent db

-- |Let the Brick run time system handle entry of text into the
-- Command Mode edit widget.
routeCommandEvent db (B.VtyEvent ev) = do
    updatedEditor <- handleEditorEvent ev (T.commandEdit db)
    B.continue $ db { T.commandEdit = updatedEditor }

routeCommandEvent db _ =
    B.continue db

---------------------------------------------------------------------
-- Events in Processing Mode

routeProcessingEvent :: Async T.Debugger -> T.Debugger -> EventHandler
-- |The <esc> key always allows the user to abort processing.
routeProcessingEvent c db (B.VtyEvent (V.EvKey V.KEsc _ )) = do
    liftIO $ A.cancel c
    B.continue $ db { T.mode    = T.NormalMode
                    , T.message = "Jump aborted" }

-- |Handle resizing of the debugger terminal window.
routeProcessingEvent _ db (B.VtyEvent (V.EvResize w h) ) =
    B.continue . D.resize w h $ db

-- |This handler is called when computation has completed and the
-- Processing Mode should be terminated.
routeProcessingEvent c _ (B.AppEvent T.ComputationDone) =
    liftIO ( A.wait c ) >>= B.continue

routeProcessingEvent _ db _ =
    B.continue db

-- =============================================================== --
-- Event handlers convert user input such as key-presses or command
-- strings entered in Command Mode into executable commands and then
-- execute them. This basically amounts to mapping the debugger to
-- a new debugger and handing this off to the Brick run time system
-- for rendering.

---------------------------------------------------------------------
-- Event to command parsers and handlers

keyEvent :: V.Key -> T.Debugger -> DebugEventMonad
-- ^Translate key events to commands based on the key-bindings and
-- then runs the command.
keyEvent k db = runCommand ( parseKey k (T.mode db) (T.wgtFocus db) ) db

commandEvent :: T.Debugger -> DebugEventMonad
-- ^Read command string from the Command Mode Edit widget, resets the
-- edit widget, parse the input to an executable command and run it.
commandEvent db = runCommand ( parseCommand cmd ) db0
    where cmd = words . unlines . getEditContents . T.commandEdit $ db
          db0 = db { T.commandEdit = editor T.CommandWgt (Just 1) ""
                   , T.mode        = T.NormalMode }

---------------------------------------------------------------------
-- Central handler for parsed commands

runCommand :: T.DebuggerCommand -> T.Debugger -> DebugEventMonad
-- ^Take a parsed command and execute it on the debugger supplying
-- the resulting, updated debugger back to the Brick run time system.
runCommand (T.PureCmd f     ) db = B.continue . f $ db
runCommand (T.QuitCmd       ) db = B.halt db
runCommand (T.ErrorCmd e    ) db = B.continue $ db { T.message = e }
runCommand (T.SimpleIOCmd f ) db = liftIO ( runInIO f db ) >>= B.continue
runCommand (T.ComplexIOCmd f) db = B.suspendAndResume $ runInIO f db
runCommand (T.TandemCmd f   ) db = runInTandem f db
runCommand (T.HScrollCmd w n) db = B.hScrollBy (B.viewportScroll w) n
                                   >> B.continue db
runCommand (T.VScrollCmd w n) db = B.vScrollBy (B.viewportScroll w) n
                                   >> B.continue db

runInIO :: (T.Debugger -> T.ErrorIO T.Debugger) -> T.Debugger -> IO T.Debugger
-- ^Some commands may need to be run in the T.ErrorIO monad transfer
-- stack. runInIO handles this and returns the resulting, updated
-- debugger in the IO monad where it can be easily handed off to the
-- Brick run time system.
runInIO f db = runExceptT (f db) >>= pure . either err id
    where err msg = db { T.message = msg }

runInTandem :: (T.Debugger -> T.Debugger) -> T.Debugger -> DebugEventMonad
-- ^Prepare a compuation for execution in its own tandem thread and
-- place the defugger into Processing Mode wrapping the threaded
-- computation. When the computation completes, it will supply a
-- T.ComputationDone event to the event queue.
runInTandem go db = do
    let jumpMessage = "Esc to abort jump..."
    c <- liftIO . A.async $ do let !db' = go db
                               writeBChan (T.channel db) T.ComputationDone
                               if T.message db' == jumpMessage
                                  then pure . D.noMessage $ db'
                                  else pure db'
    B.continue $ db { T.mode    = T.ProcessingMode c
                    , T.message = jumpMessage
                    }
