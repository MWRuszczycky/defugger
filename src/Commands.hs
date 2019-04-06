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
lookupCmd ("set":cs) = T.PureCmd $ setCmd cs
lookupCmd _          = T.PureCmd $ errorCmd "Command unrecognized"

-- =============================================================== --
-- Commands

errorCmd :: T.ErrString -> T.Debugger -> T.Debugger
errorCmd err db = db { T.message = err }

setCmd :: [String] -> T.Debugger -> T.Debugger
setCmd ("hex":_)   = noMessage . D.changeFormat T.Hex
setCmd ("dec":_)   = noMessage . D.changeFormat T.Dec
setCmd ("ascii":_) = noMessage . D.changeFormat T.Asc
setCmd (x:_)       = errorCmd $ "Cannot set property " ++ x
setCmd []          = errorCmd $ "Nothing to set"

-- =============================================================== --
-- Helpers

noMessage :: T.Debugger -> T.Debugger
noMessage db = db { T.message = "" }
