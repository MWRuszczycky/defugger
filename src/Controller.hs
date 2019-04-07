{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Rank2Types        #-}

module Controller
    ( routeEvent
    ) where

import qualified Graphics.Vty    as V
import qualified Brick           as B
import qualified Model.Types     as T
import qualified Model.Debugger  as D
import Control.Monad.IO.Class           ( liftIO            )
import Commands                         ( getCommand        )
import Brick.Widgets.Edit               ( editor
                                        , getEditContents
                                        , handleEditorEvent )

-- =============================================================== --
-- Local helper type synonyms

type DebugEventMonad = B.EventM T.WgtName (B.Next T.Debugger)
type EventHandler    = forall e. B.BrickEvent T.WgtName e -> DebugEventMonad

-- =============================================================== --
-- Event routers

routeEvent :: T.Debugger -> EventHandler
routeEvent db ev = case (T.mode db, T.wgtFocus db) of
                        (T.NormalMode,  T.ProgramWgt) -> routePNrm db ev
                        (T.CommandMode, _           ) -> routeCmd db ev
                        _                             -> routeDef db ev

routeDef :: T.Debugger -> EventHandler
-- ^Default router.
routeDef db (B.VtyEvent (V.EvKey V.KEsc _)) = B.halt db
routeDef db (B.VtyEvent (V.EvKey k  ms   )) = B.continue . dKeyEv k ms $ db
routeDef db (B.VtyEvent (V.EvResize w h  )) = B.continue . D.resize w h $ db
routeDef db _                               = B.continue db

routePNrm :: T.Debugger -> EventHandler
-- ^Routes events under debugger normal mode with program focus.
routePNrm db (B.VtyEvent (V.EvKey V.KEsc [])) = B.halt db
routePNrm db (B.VtyEvent (V.EvKey k ms)     ) = B.continue . pKeyEv k ms $ db
routePNrm db (B.VtyEvent (V.EvResize w h)   ) = B.continue . D.resize w h $ db
routePNrm db _                                = B.continue db

routeCmd :: T.Debugger -> EventHandler
-- ^Routes events under debugger command mode.
routeCmd db (B.VtyEvent (V.EvKey V.KEsc []))  = abortToNormalMode db
routeCmd db (B.VtyEvent (V.EvResize w h)   )  = B.continue . D.resize w h $ db
routeCmd db (B.VtyEvent (V.EvKey V.KEnter _)) = handleCommand db
routeCmd db (B.VtyEvent ev)                   = manageCommandEntry db ev
routeCmd db _                                 = B.continue db

-- =============================================================== --
-- Normal mode event handlers

---------------------------------------------------------------------
-- Default

dKeyEv :: V.Key -> [V.Modifier] -> T.Debugger -> T.Debugger
dKeyEv (V.KChar '\t') _ db = db { T.wgtFocus = D.nextWidget . T.wgtFocus $ db }
dKeyEv _              _ db = db

---------------------------------------------------------------------
-- With Program-UI focus

pKeyEv :: V.Key -> [V.Modifier] -> T.Debugger -> T.Debugger
  -- Cursor movements
pKeyEv V.KRight       _ db = D.moveCursorRight db
pKeyEv V.KLeft        _ db = D.moveCursorLeft db
pKeyEv V.KUp          _ db = D.moveCursorUp db
pKeyEv V.KDown        _ db = D.moveCursorDown db
pKeyEv (V.KChar 'h')  _ db = D.moveCursorLeft db
pKeyEv (V.KChar 'l')  _ db = D.moveCursorRight db
pKeyEv (V.KChar 'k')  _ db = D.moveCursorUp db
pKeyEv (V.KChar 'j')  _ db = D.moveCursorDown db
pKeyEv (V.KChar 't')  _ db = D.moveCursorRight db
  -- Program position movements
pKeyEv (V.KChar ' ')  _ db = D.stepForward db
pKeyEv V.KBS          _ db = D.stepBackward db
pKeyEv (V.KChar 'H')  _ db = D.stepBackward db
pKeyEv (V.KChar 'T')  _ db = D.stepForward db
pKeyEv (V.KChar 'L')  _ db = D.stepForward db
pKeyEv (V.KChar 'J')  _ db = D.jumpForward db
pKeyEv (V.KChar 'K')  _ db = D.jumpBackward db
  -- Entering command mode
pKeyEv (V.KChar ':' ) _ db = db { T.mode = T.CommandMode }
  -- Tabbing between widgets
pKeyEv (V.KChar '\t') _ db = db { T.wgtFocus = D.nextWidget . T.wgtFocus $ db }
pKeyEv _              _ db = db

-- =============================================================== --
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
-- ^Read the command entered in normal mode and execute it.
handleCommand db =
    let db' = db { T.commandEdit = editor T.CommandWgt (Just 1) ""
                 , T.mode        = T.NormalMode }
    in  case getCommand . unlines . getEditContents . T.commandEdit $ db of
             T.PureCmd f      -> B.continue . f $ db'
             T.SimpleIOCmd f  -> liftIO ( f db') >>= B.continue
             T.ComplexIOCmd f -> B.suspendAndResume $ f db'
             T.QuitCmd        -> B.halt db'
