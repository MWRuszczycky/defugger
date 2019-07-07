module UtilitiesTest
    ( newDebugger
    , checkDebugger
    , tempTestDir
    , getTempTestPath
    , manageTempTestDir
    ) where

import qualified Model.Types                as T
import qualified Model.Debugger.Debugger    as D
import qualified Brick.BChan                as Br
import qualified Controller.Loader          as L
import qualified Data.ByteString            as BS
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
