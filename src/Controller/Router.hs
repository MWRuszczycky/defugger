{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns      #-}

module Controller.Router
    ( routeEvent
      -- IO functions
    , runInIO       -- exported for testing purposes
    , runInTandem   -- exported for testing purposes
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
import Brick.Widgets.Edit                       ( handleEditorEvent )

-- =============================================================== --
-- Local helper type synonyms

type DebugEventMonad = B.EventM T.WgtName (B.Next T.Debugger)
type EventHandler    = B.BrickEvent T.WgtName T.DebugEvent -> DebugEventMonad

-- =============================================================== --
-- Most events can be handled using key-bindings to map key events to
-- commands for execution. The general router (routeGeneral) handles
-- these events as well as resize events. This applies to Normal Mode
-- and Help Mode. In contrast, Command Mode and Processing Mode need
-- to be handeled separately. Command Mode relies on a Brick edit
-- widget for command input, so we want to let it deal with most
-- events before getting the user input and translating this to a
-- command for execution. Processing Mode is used when a potentially
-- long or non-halting compution is to be run and we need to do so in
-- a separate thread so it can be aborted if necessary.

routeEvent :: T.Debugger -> EventHandler
routeEvent db ev = case T.mode db of
                        T.CommandMode      -> routeCommandMode db ev
                        T.ProcessingMode x -> routeProcessingMode x db ev
                        _                  -> routeGeneral db ev

-- =============================================================== --
-- General event router for Normal and Help Modes

routeGeneral :: T.Debugger -> EventHandler
routeGeneral db (B.VtyEvent (V.EvKey k _)) =
    let cmd = parseKey k (T.mode db) (T.wgtFocus db)
    in  runCommand cmd db

routeGeneral db (B.VtyEvent (V.EvResize w h) ) =
    B.continue . D.resize w h $ db

routeGeneral db _ =
    B.continue db

-- =============================================================== --
-- Events in Command Mode

routeCommandMode :: T.Debugger -> EventHandler
-- |The <esc> key aborts entry of commands and Command Mode.
routeCommandMode db (B.VtyEvent (V.EvKey V.KEsc _ )) =
    B.continue . D.noMessage . D.resetCommandEdit $ db

-- |Handle resizing of the debugger terminal window.
routeCommandMode db (B.VtyEvent (V.EvResize w h) ) =
    B.continue . D.resize w h $ db

-- |Handle the command when the user presses the <enter> key. This
-- returns the debugger to Normal Mode.
routeCommandMode db (B.VtyEvent (V.EvKey V.KEnter _ )) =
    let cmd = D.getCommandFromEdit db
    in  runCommand ( parseCommand cmd ) . D.resetCommandEdit $ db

-- |Let the Brick run time system handle entry of text into the
-- Command Mode edit widget.
routeCommandMode db (B.VtyEvent ev) = do
    updatedEditor <- handleEditorEvent ev (T.commandEdit db)
    B.continue $ db { T.commandEdit = updatedEditor }

routeCommandMode db _ =
    B.continue db

-- =============================================================== --
-- Events in Processing Mode

routeProcessingMode :: Async T.Debugger -> T.Debugger -> EventHandler
-- |The <esc> key always allows the user to abort processing.
routeProcessingMode asyncDB db (B.VtyEvent (V.EvKey V.KEsc _ )) = do
    liftIO $ A.cancel asyncDB
    B.continue $ db { T.mode    = T.NormalMode
                    , T.message = "Jump aborted" }

-- |Handle resizing of the debugger terminal window.
routeProcessingMode _ db (B.VtyEvent (V.EvResize w h) ) =
    B.continue . D.resize w h $ db

-- |This handler is called when a computation has completed and the
-- Processing Mode should be terminated. The new debugger produced
-- from the computation is used to replace the current one.
routeProcessingMode asyncDB _ (B.AppEvent T.ComputationDone) =
    liftIO ( A.wait asyncDB ) >>= B.continue

routeProcessingMode _ db _ =
    B.continue db

-- =============================================================== --
-- Running commands translated from user input events

runCommand :: T.DebuggerCommand -> T.Debugger -> DebugEventMonad
-- ^Take a parsed command and execute it on the debugger supplying
-- the resulting, updated debugger back to the Brick run time system.
runCommand (T.PureCmd f     ) db = B.continue . f $ db
runCommand (T.QuitCmd       ) db = B.halt db
runCommand (T.ErrorCmd e    ) db = B.continue $ db { T.message = e }
runCommand (T.SimpleIOCmd f ) db = liftIO ( runInIO f db ) >>= B.continue
runCommand (T.ComplexIOCmd f) db = B.suspendAndResume $ runInIO f db
runCommand (T.TandemCmd f   ) db = liftIO ( runInTandem f db ) >>= B.continue
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

runInTandem :: (T.Debugger -> T.Debugger) -> T.Debugger -> IO T.Debugger
-- ^Prepare a compuation for execution in its own tandem thread and
-- place the defugger into Processing Mode wrapping the threaded
-- computation. When the computation completes, it will supply a
-- T.ComputationDone event to the event queue.
runInTandem go db = do
    let jumpMessage = "Esc to abort jump..."
    asyncDB <- A.async $ do let !newDB = go db
                            writeBChan (T.channel db) T.ComputationDone
                            if T.message newDB == jumpMessage
                               then pure . D.noMessage $ newDB
                               else pure newDB
    pure $ db { T.mode    = T.ProcessingMode asyncDB
              , T.message = jumpMessage
              }
