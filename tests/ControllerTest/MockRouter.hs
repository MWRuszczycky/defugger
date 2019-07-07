{-# LANGUAGE OverloadedStrings #-}

module ControllerTest.MockRouter
    ( mockCommandEdit
    , routeCommandModeAsSimpleIO
    ) where

import qualified Model.Types                as T
import qualified Brick.Widgets.Edit         as Br
import qualified Controller.CommandBindings as CB
import qualified Controller.Router          as CR
import qualified Model.Debugger.Debugger    as D
import Data.Text                                  ( Text       )

-- =============================================================== --
-- Mocking command entry to the edit widget. Normally the user would
-- do this via functions supplied by Brick and its run time system.

mockCommandEdit :: Text -> T.Debugger -> T.Debugger
mockCommandEdit content db =
    db { T.commandEdit = Br.editorText T.CommandWgt (Just 1) content }

-- =============================================================== --
-- Mocking command handling in Controller to bypass the Brick
-- runtime system.

-- The routeCommandModeAs... functions should mock the
-- Controller.Router.routeCommandMode function for a given command
-- type that is expected given the state of the command mode edit
-- widget. This widget state is usually modified by the user via the
-- Brick run time system. We mock this using the mockCommandEdit
-- function, which is defined above.

routeCommandModeAsSimpleIO :: T.Debugger -> IO T.Debugger
routeCommandModeAsSimpleIO db = do
    case CB.parseCommand . D.getCommandFromEdit $ db of
         T.SimpleIOCmd f -> mockRun (T.SimpleIOCmd f) . D.resetCommandEdit $ db
         wrongCmd        -> parsedCommandError "SimpleIOCmd" wrongCmd

mockRun :: T.DebuggerCommand -> T.Debugger -> IO T.Debugger
-- ^This mocks Controller.Router.runCommand using the IO monad
-- directly instead of the Brick EventM monad.
mockRun (T.PureCmd f     ) db = pure . f $ db
mockRun (T.QuitCmd       ) db = pure db
mockRun (T.ErrorCmd e    ) db = pure $ db { T.message = e }
mockRun (T.SimpleIOCmd f ) db = CR.runInIO f db
mockRun (T.ComplexIOCmd f) db = CR.runInIO f db
mockRun (T.TandemCmd f   ) db = CR.runInTandem f db
mockRun (T.HScrollCmd _ _) db = pure db
mockRun (T.VScrollCmd _ _) db = pure db

parsedCommandError :: String -> T.DebuggerCommand -> IO a
parsedCommandError correct received =
    error $ "Parsed DebuggerCommand Error: Expected " ++ correct
            ++ " but got " ++ show received
