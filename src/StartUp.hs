module StartUp
    ( -- Running the interperter mode
      interpreter
    , endInterpreter
    , formatOutput
      --  Running the debugger mode mode
    , debugger
    , endDebugger
      -- Command line arguments parsing
    , getOptions
    ) where

-- =============================================================== --
-- Central controller for figuring out what the defugger is going  --
-- to do once it starts up.                                        --
-- =============================================================== --

import qualified Graphics.Vty    as V
import qualified Data.ByteString as BS
import qualified Brick           as B
import qualified Model.Types     as T
import System.Posix.Env                 ( putEnv        )
import Control.Monad.Except             ( liftEither
                                        , lift          )
import Model.Interpreter                ( runProgram    )
import Model.Parser                     ( parse         )
import View.View                        ( drawUI        )
import View.Core                        ( attributes    )
import Controller.Router                ( routeEvent    )
import Controller.Loader                ( initComputer
                                        , initDebugger
                                        , getScript
                                        , getDict
                                        , getInput      )

-- =============================================================== --
-- Running the interpreter mode

interpreter :: T.DefuggerOptions -> T.ErrorIO T.Computer
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

-- =============================================================== --
-- Running the debugger mode

debugger :: T.DefuggerOptions -> T.ErrorIO T.Debugger
debugger opts = do
    lift . setTerminal $ opts
    st0 <- initDebugger opts =<< lift getTerminalDimensions
    lift . B.defaultMain initApp $ st0

endDebugger :: Either T.ErrString T.Debugger -> IO ()
endDebugger (Left  e) = putStrLn $ "Error: " ++ e
endDebugger (Right _) = putStrLn "Defugger completed with no errors."

getTerminalDimensions :: IO (Int, Int)
getTerminalDimensions = V.outputForConfig V.defaultConfig >>= V.displayBounds

setTerminal :: T.DefuggerOptions -> IO ()
setTerminal opts = putEnv $ "TERM=" ++ T.terminal opts

-- =============================================================== --
-- Command line arguments parsing

getOptions :: [String] -> T.ErrorIO T.DefuggerOptions
getOptions ("--run":xs) = pure $
    defOptions { T.runMode = T.RunInterpreter
               , T.args    = xs }
getOptions xs = pure $
    defOptions { T.runMode = T.RunDebugger
               , T.args    = xs
               }

defOptions :: T.DefuggerOptions
defOptions = T.DefuggerOptions {
      T.runMode  = T.RunInterpreter
    , T.args     = []
    , T.terminal = "xterm-256color"
    }

-- =============================================================== --
-- Brick app initialization

initApp :: B.App T.Debugger e T.WgtName
initApp = B.App { B.appDraw         = drawUI
                , B.appHandleEvent  = routeEvent
                , B.appChooseCursor = \ _ -> B.showCursorNamed T.CommandWgt
                , B.appStartEvent   = pure
                , B.appAttrMap      = const attributes
                }
