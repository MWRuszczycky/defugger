module StartUp
    ( -- Running the help mode
      displayHelp
      -- Running the interperter mode
    , interpreter
    , endInterpreter
    , formatOutput
      -- Running the debugger mode
    , debugger
    , endDebugger
      -- Command line arguments parsing
    , parseOptions
    ) where

-- =============================================================== --
-- Central controller for figuring out what the defugger is going  --
-- to do once it starts up.                                        --
-- =============================================================== --

import qualified Graphics.Vty            as V
import qualified Data.Text.IO            as Tx
import qualified Data.ByteString         as BS
import qualified Brick                   as B
import qualified Model.Types             as T
import qualified System.Console.GetOpt   as Opt
import qualified Data.Set                as Set
import qualified Model.Debugger.Debugger as D
import Data.List                                ( foldl'          )
import Brick.BChan                              ( BChan, newBChan )
import Data.Default                             ( def             )
import System.Posix.Env                         ( putEnv          )
import Control.Monad.Except                     ( liftEither
                                                , lift
                                                , runExceptT
                                                , throwError      )
import Model.Interpreter                        ( runProgram
                                                , runProgramFast  )
import Model.Parser                             ( parse, toDebug  )
import Model.CoreIO                             ( tryReadFile
                                                , tryWriteBytes
                                                , tryReadBytes    )
import View.View                                ( drawUI          )
import View.Core                                ( attributes      )
import View.Help                                ( startHelp       )
import Controller.Router                        ( routeEvent      )
import Controller.Loader                        ( initComputer
                                                , initDebugger    )

-- =============================================================== --
-- Brick app initialization

initApp :: B.App T.Debugger T.DebugEvent T.WgtName
initApp = B.App { B.appDraw         = drawUI
                , B.appHandleEvent  = routeEvent
                , B.appChooseCursor = \ _ -> B.showCursorNamed T.CommandWgt
                , B.appStartEvent   = pure
                , B.appAttrMap      = const attributes
                }

-- =============================================================== --
-- Running help mode: Just print startup help to standard out.

displayHelp :: IO ()
displayHelp = Tx.putStr . startHelp $ startOptions

-- =============================================================== --
-- Running the interpreter mode

type Result = (T.Computer, T.DBProgram, Set.Set Int, BS.ByteString)

interpreter :: T.DefuggerOptions -> T.ErrorIO Result
interpreter opts = do
    let missingErr = "BF script file required."
    s <- maybe (throwError missingErr) tryReadFile . T.pathToScript $ opts
    i <- maybe (pure BS.empty) tryReadBytes . T.pathToInput $ opts
    p <- liftEither . parse def $ s
    let (brks, dbProg) = toDebug p
        run            = if T.runFast opts then runProgramFast else runProgram
    c <- liftEither . run ( initComputer i ) $ p
    pure (c, dbProg, brks, i)

endInterpreter :: T.DefuggerOptions -> Either T.ErrString Result -> IO ()
endInterpreter _    (Left  e          ) = putStrLn errMsg
    where errMsg = "Error: " ++ e ++ "\ntry: defugger --help"
endInterpreter opts (Right r@(c,_,_,_)) = do
    putStrLn . formatOutput . T.output $ c
    case T.savePath opts of
         Nothing -> pure ()
         Just fp -> do et <- runExceptT . tryWriteBytes fp . D.encodeResult $ r
                       case et of
                            Right _ -> putStrLn $ "\nResult saved to " ++ fp
                            Left e  -> putStrLn $ "\nCannot save to "
                                                 ++ fp ++ ": " ++ e

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
endDebugger (Left  e) = putStrLn $ "Error: " ++ e ++ "\ntry: defugger --help"
endDebugger (Right _) = putStrLn "Defugger completed with no errors."

getTerminalDimensions :: IO (Int, Int)
getTerminalDimensions = V.outputForConfig V.defaultConfig >>= V.displayBounds

-- =============================================================== --
-- Command line arguments parsing and Defugger startup initialization

parseOptions :: [String] -> T.ErrorIO T.DefuggerOptions
parseOptions args =
    case Opt.getOpt Opt.Permute startOptions args of
         ( opts, args, [] ) -> let dfOpts = foldl' ( flip ($) ) def opts
                               in  pure $ configure args dfOpts
         ( _   , _   , es ) -> liftEither . Left . unlines $ es

---------------------------------------------------------------------
-- Configure the Defugger startup options based on the user input

configure :: [String] -> T.DefuggerOptions -> T.DefuggerOptions
configure (x:y:_) opts = opts { T.pathToScript = Just x
                              , T.pathToInput  = Just y }
configure (x:_)   opts = opts { T.pathToScript = Just x }
configure _       opts = opts

---------------------------------------------------------------------
-- Description of all the available start-up options

startOptions :: [ Opt.OptDescr (T.DefuggerOptions -> T.DefuggerOptions) ]
startOptions =
    [ Opt.Option "h" ["help"]
          ( Opt.NoArg ( \ opts -> opts { T.runMode = T.RunHelp } ) )
          "Display help information for running the Defugger."
    , Opt.Option "f" ["fast"]
          ( Opt.NoArg ( \ opts -> opts { T.runFast = True      } ) )
          "Use the faster interpreter, but ignore break points."
    , Opt.Option "r" ["run"]
          ( Opt.NoArg ( \ opts -> opts { T.runMode = T.RunInterpreter } ) )
          "Run the BF interpreter on SCRIPT with input INPUT."
    , Opt.Option "s" ["save"]
          ( Opt.ReqArg ( \ path opts -> opts { T.runMode  = T.RunInterpreter
                                             , T.savePath = Just path } )
                       "PATH" )
          ( "Same as --run, but save the computer state to PATH.\n"
            <> "This file can then be opened in debugger mode using\n"
            <> "the :open command." )
    , Opt.Option "t" ["terminal"]
          ( Opt.ReqArg ( \ term opts -> opts { T.terminal = term } )
                       "TERM" )
          "Set the TERM terminal-ID for the debugger TUI."
    ]
