{-# LANGUAGE OverloadedStrings #-}

module Controller.Commands
    ( getCommand
    ) where

import qualified Data.Vector    as Vec
import qualified Model.Types    as T
import qualified Model.Debugger as D
import Text.Read                     ( readMaybe )
import Data.Text                     ( Text      )
import Data.List                     ( find      )
import Model.Utilities               ( chunksOf  )

-- =============================================================== --
-- Command hub and router

getCommand :: [String] -> T.DebuggerCommand
getCommand []     = T.PureCmd $ id
getCommand (x:xs) = maybe err go . find ( elem x . T.cmdNames ) $ hub
    where err  = T.ErrorCmd "Command unrecognized"
          go c = T.cmd c xs

hub :: [T.Command]
-- ^Organizes all the commands that can be run from the debugger.
hub = [ T.Command setNames   setCmd   setCmdSHelp   setCmdLHelp
      , T.Command unsetNames unsetCmd unsetCmdSHelp unsetCmdLHelp
      , T.Command writeNames writeCmd writeCmdSHelp writeCmdLHelp
      , T.Command quitNames  quitCmd  quitCmdSHelp  quitCmdLHelp
      ]

-- =============================================================== --
-- Commands

---------------------------------------------------------------------
-- set

setNames :: [String]
setNames = [ "set", "s" ]

setCmdSHelp, setCmdLHelp :: Text
setCmdSHelp = "sets a debugger property"
setCmdLHelp = "long help for set command"

setCmd :: T.DebuggerArgCommand
setCmd ("hex":_)     = setHex
setCmd ("dec":_)     = setDec
setCmd ("ascii":_)   = setAsc
setCmd ("break":_)   = setBreak
setCmd ("width":x:_) = setWidth x
setCmd (x:_)         = T.ErrorCmd $ "Cannot set property " ++ x
setCmd []            = T.ErrorCmd   "Nothing to set"

setHex, setDec, setAsc :: T.DebuggerCommand
setHex = T.PureCmd  $ D.noMessage . D.changeFormat T.Hex
setDec = T.PureCmd  $ D.noMessage . D.changeFormat T.Dec
setAsc = T.PureCmd  $ D.noMessage . D.changeFormat T.Asc

setBreak :: T.DebuggerCommand
setBreak = T.PureCmd  $ D.noMessage . D.setBreakPoint

setWidth :: String -> T.DebuggerCommand
setWidth x = maybe err (T.PureCmd . go) . readMaybe $ x
    where err  = T.ErrorCmd $ "Cannot set width to " ++ x
          go n | n < 10    = \ db -> db { T.message = "Invalid width" }
               | otherwise = \ db -> db { T.message = ""
                                        , T.progWidth = n }

---------------------------------------------------------------------
-- unset

unsetNames :: [String]
unsetNames = [ "unset", "u" ]

unsetCmdSHelp, unsetCmdLHelp :: Text
unsetCmdSHelp = "unsets a debugger property"
unsetCmdLHelp = "long help for unset command"

unsetCmd :: T.DebuggerArgCommand
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

writeCmd :: T.DebuggerArgCommand
writeCmd []    = T.SimpleIOCmd writeScript
writeCmd (x:_) = T.SimpleIOCmd $
    \ db -> writeScript $ db { T.savePath = Just x }

writeScript :: T.Debugger -> IO T.Debugger
writeScript db = do
    let mbFp = T.savePath db
    case mbFp of
         Nothing -> pure $ db { T.message = "Save path required" }
         Just fp -> do let x = formatScript (T.progWidth db) . T.program $ db
                       writeFile fp x
                       pure $ db { T.message = "Saved to " ++ fp
                                 , T.unsaved = False
                                 }

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

quitCmd :: T.DebuggerArgCommand
quitCmd _ = T.QuitCmd
