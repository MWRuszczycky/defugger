module StartUp
    ( getOptions
    , interpreter
    , debugger
    , endInterpreter
    , endDebugger
    , formatOutput
    ) where

import qualified Data.ByteString as BS
import qualified Brick           as B
import qualified Model.Types     as T
import System.Posix.Env                 ( putEnv        )
import Control.Monad.Except             ( ExceptT (..)
                                        , liftEither
                                        , lift          )
import Loader                           ( initComputer
                                        , initDebugger
                                        , getScript
                                        , getDict
                                        , getInput      )
import Model.Compiler                   ( runProgram    )
import Model.Parser                     ( parse         )
import Controller                       ( routeEvent    )
import View                             ( drawUI
                                        , attributes    )

---------------------------------------------------------------------
-- Running the interpreter mode

interpreter :: T.DefuggerOptions -> ExceptT T.ErrString IO T.Computer
interpreter opts = do
    s <- getScript opts
    d <- getDict   opts
    x <- getInput  opts
    liftEither $ parse d s >>= runProgram ( initComputer x )

endInterpreter :: Either T.ErrString T.Computer -> IO ()
endInterpreter (Left e)  = putStrLn $ "Error: " ++ e
endInterpreter (Right c) = putStrLn . formatOutput . T.output $ c

formatOutput :: BS.ByteString -> String
formatOutput = map ( toEnum . fromIntegral ) . BS.unpack

---------------------------------------------------------------------
-- Running the debugger mode

debugger :: T.DefuggerOptions -> ExceptT T.ErrString IO T.Debugger
debugger opts = do
    lift . setTerminal $ opts
    st0 <- initDebugger  opts
    lift . B.defaultMain initApp $ st0

setTerminal :: T.DefuggerOptions -> IO ()
setTerminal opts = putEnv $ "TERM=" ++ T.terminal opts

endDebugger :: Either T.ErrString T.Debugger -> IO ()
endDebugger (Left  e) = putStrLn $ "Error: " ++ e
endDebugger (Right _) = putStrLn "Defugger completed with no errors."

---------------------------------------------------------------------
-- Command line arguments parsing

getOptions :: [String] -> IO T.DefuggerOptions
getOptions ("--run":xs) = pure $
    defOptions { T.mode = T.Interpreter
               , T.args = xs }
getOptions xs = pure $
    defOptions { T.mode = T.DebugMode
               , T.args = xs
               }

defOptions :: T.DefuggerOptions
defOptions = T.DefuggerOptions {
      T.mode     = T.Interpreter
    , T.args     = []
    , T.terminal = "xterm-256color"
    }

---------------------------------------------------------------------
-- Brick app initialization

initApp :: B.App T.Debugger e ()
initApp = B.App { B.appDraw         = drawUI
                , B.appHandleEvent  = routeEvent
                , B.appChooseCursor = B.neverShowCursor
                , B.appStartEvent   = pure
                , B.appAttrMap      = const attributes
                }
