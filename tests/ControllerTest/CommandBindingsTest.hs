module ControllerTest.CommandBindingsTest
    ( spec
    ) where

import qualified Model.Types               as T
import qualified UtilitiesTest             as U
import qualified ControllerTest.MockRouter as Mc
import qualified Data.Text.IO              as Tx
import System.Directory                          ( doesPathExist              )
import Test.Hspec                                ( Spec, describe, around_
                                                 , it, shouldBe, shouldReturn )

spec :: Spec
spec = around_ U.manageTempTestDir $ do
    describe "<write> command" $ do
        it "Saves a script correctly with default path"
            testWriteDefaultPath
        it "Saves a script with a newly supplied path"
            testWriteNewPath
        it "Does not save a script without a valid path"
            testWriteNoPath

-- =============================================================== --
-- Testing the <write> command

testWriteDefaultPath :: IO ()
testWriteDefaultPath = do
    db0 <- U.newDebugger (Just "tests/files/HelloWorld.bf") Nothing
    let tempPath = U.getTempTestPath "HelloWorld-temp.bf"
        command  = ["write"]
    db1 <- Mc.handleAsSimpleIO command db0 { T.scriptPath = Just tempPath }
    T.message db1 `shouldBe` "saved to " ++ tempPath
    result   <- Tx.readFile tempPath
    expected <- Tx.readFile "tests/files/TestWriteDefaultPath.bf"
    result `shouldBe` expected

testWriteNewPath :: IO ()
testWriteNewPath = do
    db0 <- U.newDebugger (Just "tests/files/HelloWorld.bf") Nothing
    let oldTempPath = U.getTempTestPath "HelloWorld-old.bf"
        newTempPath = U.getTempTestPath "HelloWorld-new.bf"
        command     = [ "write " ++ newTempPath ]
    db1 <- Mc.handleAsSimpleIO command db0 { T.scriptPath = Just oldTempPath }
    T.message db1 `shouldBe` "saved to " ++ newTempPath
    result   <- Tx.readFile newTempPath
    expected <- Tx.readFile "tests/files/TestWriteDefaultPath.bf"
    result `shouldBe` expected
    doesPathExist oldTempPath `shouldReturn` False

testWriteNoPath :: IO ()
testWriteNoPath = do
    db0 <- U.newDebugger Nothing Nothing
    let command = [ "write" ]
    db1 <- Mc.handleAsSimpleIO command db0
    T.message db1 `shouldBe` "Save path required"
