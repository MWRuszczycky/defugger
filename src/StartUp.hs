module StartUp
    ( -- Running the interperter mode
      interpreter
    , endInterpreter
    , formatOutput
      --  Running the debugger mode mode
    , debugger
    , endDebugger
      -- Command line arguments parsing
    , parseOptions
    ) where

-- =============================================================== --
-- Central controller for figuring out what the defugger is going  --
-- to do once it starts up.                                        --
-- =============================================================== --

import qualified Graphics.Vty    as V
import qualified Data.ByteString as BS
import qualified Brick           as B
import qualified Model.Types     as T
import Brick.BChan                      ( BChan, newBChan )
import Data.Default                     ( def             )
import System.Posix.Env                 ( putEnv          )
import Control.Monad.Except             ( liftEither
                                        , lift
                                        , throwError      )
import Model.Interpreter                ( runProgram      )
import Model.Parser                     ( parse           )
import Model.CoreIO                     ( tryReadFile
                                        , tryReadBytes    )
import View.View                        ( drawUI          )
import View.Core                        ( attributes      )
import Controller.Router                ( routeEvent      )
import Controller.Loader                ( initComputer
                                        , initDebugger    )

-- =============================================================== --
-- Running the interpreter mode

interpreter :: T.DefuggerOptions -> T.ErrorIO T.Computer
interpreter opts = do
    let missingErr = "BF script file required."
    s  <- maybe (throwError missingErr) tryReadFile . T.pathToScript $ opts
    x  <- maybe (pure BS.empty) tryReadBytes . T.pathToInput $ opts
    liftEither $ parse def s >>= runProgram ( initComputer x )

endInterpreter :: Either T.ErrString T.Computer -> IO ()
endInterpreter (Left e)  = putStrLn $ "Error: " ++ e
endInterpreter (Right c) = putStrLn . formatOutput . T.output $ c

formatOutput :: BS.ByteString -> String
formatOutput = map ( toEnum . fromIntegral ) . BS.unpack

-- =============================================================== --
-- Running the debugger mode

debugger :: T.DefuggerOptions -> T.ErrorIO T.Debugger
debugger opts = do
    lift . putEnv $ "TERM=" ++ T.terminal opts
    chan <- ( lift . newBChan $ 10 ) :: T.ErrorIO ( BChan T.DebugEvent )
    st0  <- initDebugger chan opts =<< lift getTerminalDimensions
    lift . B.customMain (V.mkVty V.defaultConfig) (Just chan) initApp $ st0

endDebugger :: Either T.ErrString T.Debugger -> IO ()
endDebugger (Left  e) = putStrLn $ "Error: " ++ e
endDebugger (Right _) = putStrLn "Defugger completed with no errors."

getTerminalDimensions :: IO (Int, Int)
getTerminalDimensions = V.outputForConfig V.defaultConfig >>= V.displayBounds

-- =============================================================== --
-- Command line arguments parsing

parseOptions :: [String] -> T.ErrorIO T.DefuggerOptions
parseOptions []              = pure def
parseOptions ("--run":x:[])  = pure def { T.runMode      = T.RunInterpreter
                                        , T.pathToScript = Just x }
parseOptions ("--run":x:y:_) = pure def { T.runMode      = T.RunInterpreter
                                        , T.pathToScript = Just x
                                        , T.pathToInput  = Just y }
parseOptions (x:[])          = pure def { T.pathToScript = Just x }
parseOptions (x:y:_)         = pure def { T.pathToScript = Just x
                                        , T.pathToInput  = Just y }

-- =============================================================== --
-- Brick app initialization

initApp :: B.App T.Debugger T.DebugEvent T.WgtName
initApp = B.App { B.appDraw         = drawUI
                , B.appHandleEvent  = routeEvent
                , B.appChooseCursor = \ _ -> B.showCursorNamed T.CommandWgt
                , B.appStartEvent   = pure
                , B.appAttrMap      = const attributes
                }
