{-# LANGUAGE OverloadedStrings #-}

module Controller.Commands
    ( parseCommand
    , commands
    ) where

-- =============================================================== --
-- User commands that can be entered and run in debugger mode      --
-- =============================================================== --

import qualified Model.Types             as T
import qualified Data.Text               as Tx
import Control.Applicative                      ( (<|>)          )
import Data.Maybe                               ( listToMaybe    )
import Data.Foldable                            ( toList         )
import Control.Monad.Except                     ( throwError     )
import Data.Text                                ( Text           )
import Data.List                                ( find           )
import Model.Utilities                          ( chunksOf       )
import Model.CoreIO                             ( tryWriteFile   )
import Controller.Settings                      ( parseSet
                                                , parseUnset     )
import Controller.Loader                        ( reloadDebugger
                                                , resetDebugger  )

-- =============================================================== --
-- Command hub and router

parseCommand :: [String] -> T.DebuggerCommand
parseCommand []     = T.PureCmd $ id
parseCommand (x:xs) = maybe err go . find ( elem x . T.cmdNames ) $ commands
    where err  = T.ErrorCmd "Command unrecognized"
          go c = T.cmd c xs

commands :: [T.Command]
-- ^Organizes all the commands that can be run from the debugger.
commands = [ T.Command helpNames  helpCmd  helpCmdSHelp  helpCmdLHelp
           , T.Command loadNames  loadCmd  loadCmdSHelp  loadCmdLHelp
           , T.Command resetNames resetCmd resetCmdSHelp resetCmdLHelp
           , T.Command setNames   setCmd   setCmdSHelp   setCmdLHelp
           , T.Command unsetNames unsetCmd unsetCmdSHelp unsetCmdLHelp
           , T.Command writeNames writeCmd writeCmdSHelp writeCmdLHelp
           , T.Command quitNames  quitCmd  quitCmdSHelp  quitCmdLHelp
           ]

-- =============================================================== --
-- Commands

-- help -------------------------------------------------------------

helpNames :: [String]
helpNames = [ "help", "h"]

helpCmdSHelp, helpCmdLHelp :: Text
helpCmdSHelp = "display help"
helpCmdLHelp = "long help for help command"

helpCmd :: [String] -> T.DebuggerCommand
helpCmd xs = T.PureCmd $ \ db -> db { T.mode = T.HelpMode xs }

-- load -------------------------------------------------------------

loadNames :: [String]
loadNames = [ "load", "l" ]

loadCmdSHelp, loadCmdLHelp :: Text
loadCmdSHelp = "attempt to load a BF script into the defugger"
loadCmdLHelp = "long help for load command"

loadCmd :: [String] -> T.DebuggerCommand
loadCmd []      = T.ErrorCmd "A path to a BF script must be specified"
loadCmd (x:y:_) = T.SimpleIOCmd $ reloadDebugger (Just x) (Just y)
loadCmd (x:_)   = T.SimpleIOCmd $ reloadDebugger (Just x) Nothing

-- reset ------------------------------------------------------------

resetNames :: [String]
resetNames = [ "reset", "r" ]

resetCmdSHelp, resetCmdLHelp :: Text
resetCmdSHelp = "reset the debugger to its original state"
resetCmdLHelp = "long help for reset command"

resetCmd :: [String] -> T.DebuggerCommand
resetCmd _ = T.PureCmd $ resetDebugger

-- set --------------------------------------------------------------

setNames :: [String]
setNames = [ "set", "s" ]

setCmdSHelp, setCmdLHelp :: Text
setCmdSHelp = "sets a debugger property"
setCmdLHelp = "long help for set command"

setCmd :: [String] -> T.DebuggerCommand
setCmd = either T.ErrorCmd T.PureCmd . parseSet

--setCmd :: [String] -> T.DebuggerCommand
--setCmd ("hex":_)       = setHex
--setCmd ("dec":_)       = setDec
--setCmd ("ascii":_)     = setAsc
--setCmd ("break":_)     = setBreak
--setCmd ("width":x:_)   = setWidth x
--setCmd ("history":x:_) = setHistDepth x
--setCmd (x:_)           = T.ErrorCmd $ "Cannot set property " ++ x
--setCmd []              = T.ErrorCmd   "Nothing to set"

-- unset ------------------------------------------------------------

unsetNames :: [String]
unsetNames = [ "unset", "u" ]

unsetCmdSHelp, unsetCmdLHelp :: Text
unsetCmdSHelp = "unsets a debugger property"
unsetCmdLHelp = "long help for unset command"

unsetCmd :: [String] -> T.DebuggerCommand
unsetCmd = either T.ErrorCmd T.PureCmd . parseUnset

--unsetCmd :: [String] -> T.DebuggerCommand
--unsetCmd ("break":"all":_) = T.PureCmd  $ D.noMessage . D.unsetAllBreakPoints
--unsetCmd ("break":_)       = T.PureCmd  $ D.noMessage . D.unsetBreakPoint
--unsetCmd (x:_)             = T.ErrorCmd $ "Cannot unset property " ++ x
--unsetCmd []                = T.ErrorCmd   "Nothing to unset"

-- write ------------------------------------------------------------

writeNames :: [String]
writeNames = [ "write", "w" ]

writeCmdSHelp, writeCmdLHelp :: Text
writeCmdSHelp = "write the current script"
writeCmdLHelp = "long help for write command"

writeCmd :: [String] -> T.DebuggerCommand
writeCmd xs = T.SimpleIOCmd $ \ db -> go db $ listToMaybe xs <|> T.scriptPath db
    where fmt n = unlines . chunksOf n . init . tail . concatMap show . toList
          go _  Nothing   = throwError "Save path required"
          go db (Just fp) = do let s = fmt ( T.progWidth db ) . T.program $ db
                               tryWriteFile fp (Tx.pack s)
                               pure $ db { T.message    = "saved to " ++ fp
                                         , T.scriptPath = Just fp
                                         }

-- quit -------------------------------------------------------------

quitNames :: [String]
quitNames = [ "quit", "exit", "q" ]

quitCmdSHelp, quitCmdLHelp :: Text
quitCmdSHelp = "quits the defugger"
quitCmdLHelp = "not much more to say about quitting"

quitCmd :: [String] -> T.DebuggerCommand
quitCmd _ = T.QuitCmd
