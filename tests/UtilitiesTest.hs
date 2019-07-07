module UtilitiesTest
    ( newDebugger
    , checkDebugger
    , checkCommandEdit
    , testFilesDir
    , getTestPath
    , tempTestDir
    , getTempTestPath
    , manageTempTestDir
    ) where

import qualified Model.Types                as T
import qualified Model.Debugger.Debugger    as D
import qualified Brick.BChan                as Br
import qualified Controller.Loader          as L
import qualified Data.ByteString            as BS
import Data.Text                                  ( Text                      )
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
-- After the debugger is initated, the file paths are all changed to
-- the temp directory so the original files do not get overwritten.
-- Arbitrary terminal dimensions of (160, 50) are used.
newDebugger script input = do
    let opts = def { T.pathToScript = getTestPath <$> script
                   , T.pathToInput  = getTestPath <$> input  }
    chan <- Br.newBChan 10 :: IO (Br.BChan T.DebugEvent)
    etDB <- runExceptT $ L.initDebugger chan opts (160,50)
    case etDB of
         Left err -> error $ "Cannot load a test debugger: " ++ err
         Right db -> pure $ db { T.scriptPath = getTempTestPath <$> script
                               , T.inputPath  = getTempTestPath <$> input
                               }

checkDebugger :: BS.ByteString -> BS.ByteString
                 -> String -> Int -> T.Debugger -> IO T.Debugger
-- ^Check the computer state of the debugger against expected values.
checkDebugger input output memory position db = do
    (T.input . T.computer) db        `shouldBe` input
    (T.output . T.computer) db       `shouldBe` output
    (show. T.memory . T.computer) db `shouldBe` memory
    D.getPosition db                 `shouldBe` position
    pure db

checkCommandEdit :: [Text] -> T.Debugger -> IO ()
checkCommandEdit expected db = D.getCommandFromEdit db `shouldBe` expected

-- =============================================================== --
-- Utilities for handling test files and temporary files

testFilesDir :: FilePath
testFilesDir = "tests/files/"

getTestPath :: FilePath -> FilePath
getTestPath = (++) testFilesDir

tempTestDir :: FilePath
tempTestDir = "tests/temp/"

getTempTestPath :: FilePath -> FilePath
getTempTestPath = (++) tempTestDir

manageTempTestDir :: IO () -> IO ()
manageTempTestDir = bracket_ createTemp deleteTemp
    where createTemp = deleteTemp >> createDirectory tempTestDir
          deleteTemp = do exists <- doesDirectoryExist tempTestDir
                          when exists $ removeDirectoryRecursive tempTestDir
