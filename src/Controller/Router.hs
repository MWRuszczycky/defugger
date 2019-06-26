{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Rank2Types        #-}

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
import Control.Monad.IO.Class                   ( liftIO            )
import Controller.Commands                      ( getCommand        )
import Brick.Widgets.Edit                       ( editor
                                                , getEditContents
                                                , handleEditorEvent )

-- =============================================================== --
-- Local helper type synonyms

type DebugEventMonad = B.EventM T.WgtName (B.Next T.Debugger)
type EventHandler    = forall e. B.BrickEvent T.WgtName e -> DebugEventMonad

-- =============================================================== --
-- Event router -- This is the controller entry point.

routeEvent :: T.Debugger -> EventHandler
routeEvent db ev =
    case (T.mode db, T.wgtFocus db) of
         (T.NormalMode,  T.ProgramWgt) -> routeProgramNormalEvent db ev
         (T.NormalMode,  w           ) -> routeNonProgramNormalEvent w db ev
         (T.CommandMode, _           ) -> routeCommandEvent  db ev

-- =============================================================== --
-- Events in normal mode with program focus

---------------------------------------------------------------------
-- Router

routeProgramNormalEvent :: T.Debugger -> EventHandler
routeProgramNormalEvent db (B.VtyEvent (V.EvKey V.KEsc _ )) =
    B.halt db

routeProgramNormalEvent db (B.VtyEvent (V.EvKey k ms)) =
    B.continue . keyEvent k ms $ db

routeProgramNormalEvent db (B.VtyEvent (V.EvResize w h) ) =
    B.continue . D.resize w h $ db

routeProgramNormalEvent db _ =
    B.continue db

---------------------------------------------------------------------
-- Key events

keyEvent :: V.Key -> [V.Modifier] -> T.Debugger -> T.Debugger
  -- Cursor movements
keyEvent V.KRight       _ db = D.moveCursorRight db
keyEvent V.KLeft        _ db = D.moveCursorLeft db
keyEvent V.KUp          _ db = D.moveCursorUp db
keyEvent V.KDown        _ db = D.moveCursorDown db
keyEvent (V.KChar 'h')  _ db = D.moveCursorLeft db
keyEvent (V.KChar 'l')  _ db = D.moveCursorRight db
keyEvent (V.KChar 'k')  _ db = D.moveCursorUp db
keyEvent (V.KChar 'j')  _ db = D.moveCursorDown db
keyEvent (V.KChar 't')  _ db = D.moveCursorRight db
  -- Program position movements
keyEvent (V.KChar ' ')  _ db = D.stepForward db
keyEvent V.KBS          _ db = D.stepBackward db
keyEvent (V.KPageDown)  _ db = D.jumpForward db
keyEvent (V.KPageUp  )  _ db = D.jumpBackward db
keyEvent (V.KChar 'H')  _ db = D.stepBackward db
keyEvent (V.KChar 'T')  _ db = D.stepForward db
keyEvent (V.KChar 'L')  _ db = D.stepForward db
keyEvent (V.KChar 'J')  _ db = D.jumpForward db
keyEvent (V.KChar 'K')  _ db = D.jumpBackward db
  -- Program editing
keyEvent (V.KChar 'x')  _ db = D.deleteStatementAtCursor db
keyEvent (V.KChar '<')  _ db = D.addStatementAtCursor T.DBBackup db
keyEvent (V.KChar '>')  _ db = D.addStatementAtCursor T.DBAdvance db
keyEvent (V.KChar '+')  _ db = D.addStatementAtCursor T.DBIncrement db
keyEvent (V.KChar '-')  _ db = D.addStatementAtCursor T.DBDecrement db
keyEvent (V.KChar '.')  _ db = D.addStatementAtCursor T.DBWriteOut db
keyEvent (V.KChar ',')  _ db = D.addStatementAtCursor T.DBReadIn db
keyEvent (V.KChar '[')  _ db = D.addStatementAtCursor (T.DBOpenLoop 0) db
keyEvent (V.KChar ']')  _ db = D.addStatementAtCursor (T.DBCloseLoop 0) db
  -- Entering command mode
keyEvent (V.KChar ':' ) _ db = db { T.mode = T.CommandMode }
  -- Tabbing between widgets
keyEvent (V.KChar '\t') _ db = db { T.wgtFocus = D.nextWidget . T.wgtFocus $ db }
keyEvent _              _ db = db

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

routeNonProgramNormalEvent T.OutputWgt db (B.VtyEvent (V.EvKey k _ )) =
    scroll (B.viewportScroll T.OutputWgt) k $ db

routeNonProgramNormalEvent T.InputWgt  db (B.VtyEvent (V.EvKey k _ )) =
    scroll (B.viewportScroll T.InputWgt) k $ db

routeNonProgramNormalEvent _  db _ =
    B.continue db

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
handleCommand db =
    let db'    = db { T.commandEdit = editor T.CommandWgt (Just 1) ""
                    , T.mode        = T.NormalMode }
        cmdStr = getEditContents . T.commandEdit $ db
    in  case getCommand . words . unlines $ cmdStr of
             T.PureCmd f      -> B.continue . f $ db'
             T.SimpleIOCmd f  -> liftIO ( f db') >>= B.continue
             T.ComplexIOCmd f -> B.suspendAndResume $ f db'
             T.ErrorCmd e     -> B.continue $ db' { T.message = e }
             T.QuitCmd        -> B.halt db'
