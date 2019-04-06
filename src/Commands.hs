module Commands
    ( getCommand
    ) where

import qualified Model.Types    as T
import qualified Model.Debugger as D

-- =============================================================== --
-- Command routers

getCommand :: String -> T.DebuggerCommand
getCommand s = lookupCmd . words $ s

lookupCmd :: [String] -> T.DebuggerCommand
lookupCmd ("set":cs)   = T.PureCmd $ setCmd cs
lookupCmd ("unset":cs) = T.PureCmd $ unsetCmd cs
lookupCmd ("q":_)      = T.QuitCmd
lookupCmd ("quit":_)   = T.QuitCmd
lookupCmd ("exit":_)   = T.QuitCmd
lookupCmd _            = T.PureCmd $ errorCmd "Command unrecognized"

-- =============================================================== --
-- Commands

errorCmd :: T.ErrString -> T.Debugger -> T.Debugger
errorCmd err db = db { T.message = err }

setCmd :: [String] -> T.Debugger -> T.Debugger
setCmd ("hex":_)   = noMessage . D.changeFormat T.Hex
setCmd ("dec":_)   = noMessage . D.changeFormat T.Dec
setCmd ("ascii":_) = noMessage . D.changeFormat T.Asc
setCmd ("break":_) = noMessage . D.setBreakPoint
setCmd (x:_)       = errorCmd $ "Cannot set property " ++ x
setCmd []          = errorCmd $ "Nothing to set"

unsetCmd :: [String] -> T.Debugger -> T.Debugger
unsetCmd ("break":"all":_) = noMessage . D.unsetAllBreakPoints
unsetCmd ("break":_)       = noMessage . D.unsetBreakPoint
unsetCmd (x:_)             = errorCmd $ "Cannot unset property " ++ x
unsetCmd []                = errorCmd $ "Nothing to unset"

-- =============================================================== --
-- Helpers

noMessage :: T.Debugger -> T.Debugger
noMessage db = db { T.message = "" }
