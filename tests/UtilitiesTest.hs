module UtilitiesTest
    ( newDebugger
    , checkDebugger
    , tempTestDir
    , getTempTestPath
    , manageTempTestDir
    , handleAsSimpleIO
    , handleAsError
    ) where

import qualified Model.Types                as T
import qualified Model.Debugger.Debugger    as D
import qualified Brick.BChan                as Br
import qualified Controller.Loader          as L
import qualified Data.ByteString            as BS
import qualified Controller.CommandBindings as CB
import qualified Brick.Widgets.Edit         as Br
import Control.Exception                          ( bracket_                  )
import Control.Monad                              ( when                      )
import Data.Default                               ( def                       )
import Control.Monad.Except                       ( runExceptT                )
import Test.Hspec                                 ( shouldBe                  )
import System.Directory                           ( createDirectory
                                                  , doesDirectoryExist
                                                  , removeDirectoryRecursive  )


-- =============================================================== --
-- Common utility functions for testing the Debugger.              --
-- =============================================================== --

newDebugger :: Maybe FilePath -> Maybe FilePath -> IO T.Debugger
-- ^Generate a new debugger from the given script and input files.
newDebugger script input = do
    let opts = def { T.pathToScript = script, T.pathToInput = input }
    chan <- Br.newBChan 10 :: IO (Br.BChan T.DebugEvent)
    etDB <- runExceptT $ L.initDebugger chan opts (160,50)
    case etDB of
         Left err -> error $ "Cannot load a test debugger: " ++ err
         Right db -> pure db

checkDebugger :: T.Debugger -> BS.ByteString -> BS.ByteString
                 -> String -> Int -> IO ()
-- ^Check the computer state of the dubegger against expected values.
checkDebugger db input output memory position = do
    (T.input . T.computer) db        `shouldBe` input
    (T.output . T.computer) db       `shouldBe` output
    (show. T.memory . T.computer) db `shouldBe` memory
    D.getPosition db                 `shouldBe` position

-- =============================================================== --
-- Utilities for handling a tempory test directory

tempTestDir :: FilePath
tempTestDir = "tests/temp/"

getTempTestPath :: FilePath -> FilePath
getTempTestPath = (++) tempTestDir

manageTempTestDir :: IO () -> IO ()
manageTempTestDir = bracket_ createTemp deleteTemp
    where createTemp = deleteTemp >> createDirectory tempTestDir
          deleteTemp = do exists <- doesDirectoryExist tempTestDir
                          when exists $ removeDirectoryRecursive tempTestDir

-- =============================================================== --
-- Utilities for running tests on debugger commands

mockIOHandler :: ( T.Debugger -> T.ErrorIO T.Debugger)
                 -> ( T.Debugger -> IO T.Debugger )
mockIOHandler f db = runExceptT ( f db' ) >>= pure . either ( err db' ) id
    where err dbx m = dbx { T.message = m }
          db'       = db { T.commandEdit = Br.editor T.CommandWgt (Just 1) ""
                         , T.mode        = T.NormalMode
                         }

handleAsError :: [String] -> T.Debugger -> IO T.Debugger
handleAsError commands db = do
    case CB.parseCommand . words . unlines $ commands of
         T.PureCmd _      -> error "Expected ErrorCmd command got PureCmd"
         T.ComplexIOCmd _ -> error "Expected ErrorCmd command got ComplexIOCmd"
         T.QuitCmd        -> error "Expected ErrorCmd command got QuitCmd"
         T.SimpleIOCmd _  -> error "Expected ErrorCmd command got SimpleIOCmd"
         T.TandemCmd _    -> error "Expected ErrorCmd command got TandemCmd"
         T.HScrollCmd _ _ -> error "Expected ErrorCmd command got HScrollCmd"
         T.VScrollCmd _ _ -> error "Expected ErrorCmd command got VScrollCmd"
         T.ErrorCmd e     -> pure $ db { T.message = e }

handleAsSimpleIO :: [String] -> T.Debugger -> IO T.Debugger
handleAsSimpleIO commands db = do
    case CB.parseCommand . words . unlines $ commands of
         T.PureCmd _      -> error "Expected SimpleIO command got PureCmd"
         T.ComplexIOCmd _ -> error "Expected SimpleIO command got ComplexIOCmd"
         T.ErrorCmd _     -> error "Expected SimpleIO command got ErrorCmd"
         T.QuitCmd        -> error "Expected SimpleIO command got QuitCmd"
         T.TandemCmd _    -> error "Expected SimpleIO command got TandemCmd"
         T.HScrollCmd _ _ -> error "Expected SimpleIO command got HScrollCmd"
         T.VScrollCmd _ _ -> error "Expected SimpleIO command got VScrollCmd"
         T.SimpleIOCmd f  -> mockIOHandler f db
