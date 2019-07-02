{-# LANGUAGE OverloadedStrings #-}

module Controller.Commands
    ( getCommand
    , hub
    ) where

import qualified Data.Vector             as Vec
import qualified Model.Types             as T
import qualified Model.Debugger.Debugger as D
import qualified Data.Sequence           as Seq
import Control.Monad.Except                     ( runExceptT     )
import Control.Monad                            ( guard          )
import Text.Read                                ( readMaybe      )
import Data.Text                                ( Text           )
import Data.List                                ( find           )
import Model.Utilities                          ( chunksOf       )
import Controller.Loader                        ( reloadDebugger
                                                , resetDebugger  )

-- =============================================================== --
-- Command hub and router

getCommand :: [String] -> T.DebuggerCommand
getCommand []     = T.PureCmd $ id
getCommand (x:xs) = maybe err go . find ( elem x . T.cmdNames ) $ hub
    where err  = T.ErrorCmd "Command unrecognized"
          go c = T.cmd c xs

hub :: [T.Command]
-- ^Organizes all the commands that can be run from the debugger.
hub = [ T.Command helpNames  helpCmd  helpCmdSHelp  helpCmdLHelp
      , T.Command loadNames  loadCmd  loadCmdSHelp  loadCmdLHelp
      , T.Command resetNames resetCmd resetCmdSHelp resetCmdLHelp
      , T.Command setNames   setCmd   setCmdSHelp   setCmdLHelp
      , T.Command unsetNames unsetCmd unsetCmdSHelp unsetCmdLHelp
      , T.Command writeNames writeCmd writeCmdSHelp writeCmdLHelp
      , T.Command quitNames  quitCmd  quitCmdSHelp  quitCmdLHelp
      ]

-- =============================================================== --
-- Commands

---------------------------------------------------------------------
-- help

helpNames :: [String]
helpNames = [ "help", "h"]

helpCmdSHelp, helpCmdLHelp :: Text
helpCmdSHelp = "display help"
helpCmdLHelp = "long help for help command"

helpCmd :: [String] -> T.DebuggerCommand
helpCmd _ = T.PureCmd $ \ db -> db { T.mode = T.HelpMode ["help"] }

---------------------------------------------------------------------
-- load

loadNames :: [String]
loadNames = [ "load", "l" ]

loadCmdSHelp, loadCmdLHelp :: Text
loadCmdSHelp = "attempt to load a BF script into the defugger"
loadCmdLHelp = "long help for load command"

loadCmd :: [String] -> T.DebuggerCommand
loadCmd []      = T.ErrorCmd "A path to a BF script must be specified"
loadCmd (x:y:_) = T.SimpleIOCmd $ tryLoad (Just x) (Just y)
loadCmd (x:_)   = T.SimpleIOCmd $ tryLoad (Just x) Nothing

tryLoad :: Maybe FilePath -> Maybe FilePath -> T.Debugger -> IO T.Debugger
tryLoad scriptPath inputPath db0 = do
    result <- runExceptT . reloadDebugger scriptPath inputPath $ db0
    case result of
         Left errMsg -> pure $ db0 { T.message = errMsg }
         Right db1   -> pure . D.noMessage $ db1

---------------------------------------------------------------------
-- reset

resetNames :: [String]
resetNames = [ "reset", "r" ]

resetCmdSHelp, resetCmdLHelp :: Text
resetCmdSHelp = "reset the debugger to its original state"
resetCmdLHelp = "long help for reset command"

resetCmd :: [String] -> T.DebuggerCommand
resetCmd _ = T.PureCmd $ resetDebugger

---------------------------------------------------------------------
-- set

setNames :: [String]
setNames = [ "set", "s" ]

setCmdSHelp, setCmdLHelp :: Text
setCmdSHelp = "sets a debugger property"
setCmdLHelp = "long help for set command"

setCmd :: [String] -> T.DebuggerCommand
setCmd ("hex":_)       = setHex
setCmd ("dec":_)       = setDec
setCmd ("ascii":_)     = setAsc
setCmd ("break":_)     = setBreak
setCmd ("width":x:_)   = setWidth x
setCmd ("history":x:_) = setHistDepth x
setCmd (x:_)           = T.ErrorCmd $ "Cannot set property " ++ x
setCmd []              = T.ErrorCmd   "Nothing to set"

setHex, setDec, setAsc :: T.DebuggerCommand
setHex = T.PureCmd $ D.noMessage . D.changeFormat T.Hex
setDec = T.PureCmd $ D.noMessage . D.changeFormat T.Dec
setAsc = T.PureCmd $ D.noMessage . D.changeFormat T.Asc

setBreak :: T.DebuggerCommand
setBreak = T.PureCmd  $ D.noMessage . D.setBreakPoint

setWidth :: String -> T.DebuggerCommand
setWidth x = maybe err (T.PureCmd . go) . readMaybe $ x
    where err  = T.ErrorCmd $ "Cannot set width to " ++ x
          go n | n < 10    = \ db -> db { T.message = "Invalid width" }
               | otherwise = \ db -> db { T.message = ""
                                        , T.progWidth = n }

setHistDepth :: String -> T.DebuggerCommand
setHistDepth x = maybe err T.PureCmd . go $ x
    where err  = T.ErrorCmd $ "Cannot set history reversion depth to " ++ x
          go y = do n <- readMaybe y
                    guard (n >= 0 )
                    pure $ \ db -> let h = T.history db
                                   in  db { T.message = ""
                                          , T.histDepth = n + 1
                                          , T.history = Seq.take (n+1) h
                                          }

---------------------------------------------------------------------
-- unset

unsetNames :: [String]
unsetNames = [ "unset", "u" ]

unsetCmdSHelp, unsetCmdLHelp :: Text
unsetCmdSHelp = "unsets a debugger property"
unsetCmdLHelp = "long help for unset command"

unsetCmd :: [String] -> T.DebuggerCommand
unsetCmd ("break":"all":_) = T.PureCmd  $ D.noMessage . D.unsetAllBreakPoints
unsetCmd ("break":_)       = T.PureCmd  $ D.noMessage . D.unsetBreakPoint
unsetCmd (x:_)             = T.ErrorCmd $ "Cannot unset property " ++ x
unsetCmd []                = T.ErrorCmd   "Nothing to unset"

---------------------------------------------------------------------
-- write

writeNames :: [String]
writeNames = [ "write", "w" ]

writeCmdSHelp, writeCmdLHelp :: Text
writeCmdSHelp = "write the current script"
writeCmdLHelp = "long help for write command"

writeCmd :: [String] -> T.DebuggerCommand
writeCmd []    = T.SimpleIOCmd writeScript
writeCmd (x:_) = T.SimpleIOCmd $
    \ db -> writeScript $ db { T.scriptPath = Just x }

writeScript :: T.Debugger -> IO T.Debugger
writeScript db = do
    let mbFp = T.scriptPath db
    case mbFp of
         Nothing -> pure $ db { T.message = "Save path required" }
         Just fp -> do let x = formatScript (T.progWidth db) . T.program $ db
                       writeFile fp x
                       pure $ db { T.message = "Saved to " ++ fp }

formatScript :: Int -> T.DBProgram -> String
-- ^Format a BF debug program to a string with n characters per line.
formatScript n = unlines . chunksOf n
                 . init . tail
                 . concatMap show
                 . Vec.toList

---------------------------------------------------------------------
-- quit | exit | q

quitNames :: [String]
quitNames = [ "quit", "exit", "q" ]

quitCmdSHelp, quitCmdLHelp :: Text
quitCmdSHelp = "quits the defugger"
quitCmdLHelp = "not much more to say about quitting"

quitCmd :: [String] -> T.DebuggerCommand
quitCmd _ = T.QuitCmd
