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
routeEvent db ev =
    case (T.mode db, T.wgtFocus db) of
         (T.NormalMode,  T.ProgramWgt) -> routeProgramNormalEvent db ev
         (T.NormalMode,  w           ) -> routeNonProgramNormalEvent w db ev
         (T.CommandMode, _           ) -> routeCommandEvent  db ev
         (T.ProcessingMode c, _      ) -> routeProcessingEvent c db ev
         (T.HelpMode _, _            ) -> routeHelpEvent db ev

-- =============================================================== --
-- Events in help mode

---------------------------------------------------------------------
-- Router

routeHelpEvent :: T.Debugger -> EventHandler
routeHelpEvent db (B.VtyEvent (V.EvKey V.KEsc _ )) =
    B.continue $ db { T.mode = T.NormalMode }

routeHelpEvent db (B.VtyEvent (V.EvKey (V.KChar 'q') _ )) =
    B.continue $ db { T.mode = T.NormalMode }

routeHelpEvent db (B.VtyEvent (V.EvResize w h) ) =
    B.continue . D.resize w h $ db

routeHelpEvent db (B.VtyEvent (V.EvKey k _ )) =
    scroll (B.viewportScroll T.HelpWgt) k $ db

routeHelpEvent db _ =
    B.continue db

-- =============================================================== --
-- Events in normal mode with program focus

---------------------------------------------------------------------
-- Router

routeProgramNormalEvent :: T.Debugger -> EventHandler
routeProgramNormalEvent db (B.VtyEvent (V.EvKey V.KEsc _ )) =
    B.halt db

routeProgramNormalEvent db (B.VtyEvent (V.EvKey k _)) =
    keyEvent k db

routeProgramNormalEvent db (B.VtyEvent (V.EvResize w h) ) =
    B.continue . D.resize w h $ db

routeProgramNormalEvent db _ =
    B.continue db

---------------------------------------------------------------------
-- Key events

keyEvent :: V.Key -> T.Debugger -> DebugEventMonad
keyEvent k db = case parseKey k (T.mode db) (T.wgtFocus db) of
                     T.PureCmd f      -> B.continue . f $ db
                     T.QuitCmd        -> B.halt db
                     T.ErrorCmd e     -> B.continue $ db { T.message = e }
                     T.SimpleIOCmd f  -> B.continue db
                     T.ComplexIOCmd f -> B.continue db
                     T.TandemIOCmd f  -> B.continue db

--pKeyEvent :: V.Key -> [V.Modifier] -> T.Debugger -> DebugEventMonad
--  -- Cursor movements
--pKeyEvent V.KRight       _ db = B.continue . D.moveCursorRight $ db
--pKeyEvent V.KLeft        _ db = B.continue . D.moveCursorLeft  $ db
--pKeyEvent V.KUp          _ db = B.continue . D.moveCursorUp    $ db
--pKeyEvent V.KDown        _ db = B.continue . D.moveCursorDown  $ db
--pKeyEvent (V.KChar 'h')  _ db = B.continue . D.moveCursorLeft  $ db
--pKeyEvent (V.KChar 'l')  _ db = B.continue . D.moveCursorRight $ db
--pKeyEvent (V.KChar 'k')  _ db = B.continue . D.moveCursorUp    $ db
--pKeyEvent (V.KChar 'j')  _ db = B.continue . D.moveCursorDown  $ db
--pKeyEvent (V.KChar 't')  _ db = B.continue . D.moveCursorRight $ db
--  -- Program single steps: these should be fast and will not hang
--pKeyEvent (V.KChar ' ')  _ db = B.continue . D.stepForward  $ db
--pKeyEvent V.KBS          _ db = B.continue . D.stepBackward $ db
--pKeyEvent (V.KChar 'H')  _ db = B.continue . D.stepBackward $ db
--pKeyEvent (V.KChar 'T')  _ db = B.continue . D.stepForward  $ db
--pKeyEvent (V.KChar 'L')  _ db = B.continue . D.stepForward  $ db
--  -- Progam jumps: these may be slow or non-halting
--pKeyEvent (V.KPageDown)  _ db = isolate D.jumpForward  db
--pKeyEvent (V.KPageUp  )  _ db = isolate D.jumpBackward db
--pKeyEvent (V.KChar 'J')  _ db = isolate D.jumpForward  db
--pKeyEvent (V.KChar 'K')  _ db = isolate D.jumpBackward db
--  -- Program editing
--pKeyEvent (V.KChar 'x')  _ db = B.continue . D.deleteStatementAtCursor       $ db
--pKeyEvent (V.KChar '<')  _ db = B.continue . D.addAtCursor T.DBBackup        $ db
--pKeyEvent (V.KChar '>')  _ db = B.continue . D.addAtCursor T.DBAdvance       $ db
--pKeyEvent (V.KChar '+')  _ db = B.continue . D.addAtCursor T.DBIncrement     $ db
--pKeyEvent (V.KChar '-')  _ db = B.continue . D.addAtCursor T.DBDecrement     $ db
--pKeyEvent (V.KChar '.')  _ db = B.continue . D.addAtCursor T.DBWriteOut      $ db
--pKeyEvent (V.KChar ',')  _ db = B.continue . D.addAtCursor T.DBReadIn        $ db
--pKeyEvent (V.KChar '[')  _ db = B.continue . D.addAtCursor (T.DBOpenLoop 0)  $ db
--pKeyEvent (V.KChar ']')  _ db = B.continue . D.addAtCursor (T.DBCloseLoop 0) $ db
--  -- Entering command mode
--pKeyEvent (V.KChar ':' ) _ db = B.continue $ db { T.mode = T.CommandMode }
--  -- Tabbing between widgets
--pKeyEvent (V.KChar '\t') _ db = let newFocus = D.nextWidget . T.wgtFocus $ db
--                                in  B.continue $ db { T.wgtFocus = newFocus }
--pKeyEvent _              _ db = B.continue db

-- =============================================================== --
-- Events in normal mode with non-program widget focus

---------------------------------------------------------------------
-- Router

routeNonProgramNormalEvent :: T.WgtName -> T.Debugger -> EventHandler
routeNonProgramNormalEvent _ db (B.VtyEvent (V.EvKey V.KEsc _ )) =
    B.halt db

routeNonProgramNormalEvent _ db (B.VtyEvent (V.EvResize w h )) =
    B.continue . D.resize w h $ db

routeNonProgramNormalEvent _ db (B.VtyEvent (V.EvKey (V.KChar '\t') _ )) =
    B.continue $ db { T.wgtFocus = D.nextWidget . T.wgtFocus $ db }

routeNonProgramNormalEvent _ db (B.VtyEvent (V.EvKey (V.KChar ':') _ )) =
    B.continue $ db { T.mode = T.CommandMode }

routeNonProgramNormalEvent T.MemoryWgt db (B.VtyEvent (V.EvKey k ms )) =
    mKeyEvent k ms db

routeNonProgramNormalEvent T.OutputWgt db (B.VtyEvent (V.EvKey k _ )) =
    scroll (B.viewportScroll T.OutputWgt) k $ db

routeNonProgramNormalEvent T.InputWgt  db (B.VtyEvent (V.EvKey k _ )) =
    scroll (B.viewportScroll T.InputWgt) k $ db

routeNonProgramNormalEvent _  db _ =
    B.continue db

---------------------------------------------------------------------
-- Key events in the memory-UI

mKeyEvent :: V.Key -> [V.Modifier] -> T.Debugger -> DebugEventMonad
  -- Scrolling the memory widget
mKeyEvent V.KUp         _ db = B.continue . D.scrollMemView (-1) $ db
mKeyEvent V.KDown       _ db = B.continue . D.scrollMemView ( 1) $ db
mKeyEvent (V.KChar 'k') _ db = B.continue . D.scrollMemView (-1) $ db
mKeyEvent (V.KChar 'j') _ db = B.continue . D.scrollMemView ( 1) $ db
mKeyEvent _             _ db = B.continue db

---------------------------------------------------------------------
-- Scrolling of output- and input-UI widgets

scroll :: B.ViewportScroll T.WgtName-> V.Key -> T.Debugger -> DebugEventMonad
scroll vp V.KUp         db = B.vScrollBy vp (-1) >> B.continue db
scroll vp V.KDown       db = B.vScrollBy vp ( 1) >> B.continue db
scroll vp V.KLeft       db = B.hScrollBy vp (-1) >> B.continue db
scroll vp V.KRight      db = B.hScrollBy vp ( 1) >> B.continue db
scroll vp (V.KChar 'h') db = B.hScrollBy vp (-1) >> B.continue db
scroll vp (V.KChar 'l') db = B.hScrollBy vp ( 1) >> B.continue db
scroll vp (V.KChar 'k') db = B.vScrollBy vp (-1) >> B.continue db
scroll vp (V.KChar 'j') db = B.vScrollBy vp ( 1) >> B.continue db
scroll vp (V.KChar 't') db = B.hScrollBy vp ( 1) >> B.continue db
scroll vp (V.KChar 'd') db = B.hScrollBy vp (-1) >> B.continue db
scroll _  _             db = B.continue db

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
             T.TandemIOCmd f  -> B.continue db1

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
