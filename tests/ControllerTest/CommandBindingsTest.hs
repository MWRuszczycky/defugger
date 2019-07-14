{-# LANGUAGE OverloadedStrings #-}

module ControllerTest.CommandBindingsTest
    ( spec
    ) where

import qualified Model.Types               as T
import qualified UtilitiesTest             as U
import qualified ControllerTest.MockRouter as Mc
import qualified Data.Text                 as Tx
import qualified Data.Text.IO              as Tx
import qualified Model.Debugger.Debugger   as D
import qualified Data.ByteString           as BS
import System.Directory                          ( doesPathExist              )
import Test.Hspec                                ( Spec, describe, around_
                                                 , it, shouldBe, shouldReturn )

spec :: Spec
spec = around_ U.manageTempTestDir $ do
    describe ":write command" $ do
        it "Writes a script correctly with default path"
            testWriteDefaultPath
        it "Writes a script with a newly supplied path"
            testWriteNewPath
        it "Does not write a script without a valid path"
            testWriteNoPath
    describe ":save command" $ do
        it "Saves debugger state correctly as bytestring with default path"
            testSaveDefaultPath
        it "Saves debugger state correctly as bytestring with a new path"
            testSaveNewPath
        it "Does not save debugger state without a valid path"
            testSaveNoPath

-- =============================================================== --
-- Testing the :write command

testWriteDefaultPath :: IO ()
testWriteDefaultPath = do
    -- Set up mocked debugger
    newDB <- U.newDebugger (Just "HelloWorld.bf") Nothing
    let mockDB    = Mc.mockCommandEdit "write" newDB
        Just path = T.scriptPath newDB
    -- Test execution as SimpleIO
    resultDB <- Mc.routeCommandModeAsSimpleIO mockDB
    -- Check the results
    T.message resultDB `shouldBe` "saved to " ++ path
    U.checkCommandEdit [] resultDB
    resultIO   <- Tx.readFile path
    expectedIO <- Tx.readFile . U.getTestPath $ "TestWriteDefaultPath.bf"
    resultIO `shouldBe` expectedIO

testWriteNewPath :: IO ()
testWriteNewPath = do
    -- Set up the mocked debugger
    newDB <- U.newDebugger (Just "HelloWorld.bf") Nothing
    let Just oldPath = T.scriptPath newDB
        newPath = U.getTempTestPath "HelloWorld-new.bf"
        mockDB  = Mc.mockCommandEdit ( "write " <> Tx.pack newPath ) $
                      newDB { T.scriptPath = Just oldPath }
    -- Test execution as SimpleIO
    resultDB <- Mc.routeCommandModeAsSimpleIO mockDB
    -- Check the results
    T.message resultDB `shouldBe` "saved to " ++ newPath
    U.checkCommandEdit [] resultDB
    resultIO   <- Tx.readFile newPath
    expectedIO <- Tx.readFile . U.getTestPath $ "TestWriteDefaultPath.bf"
    resultIO `shouldBe` expectedIO
    doesPathExist oldPath `shouldReturn` False

testWriteNoPath :: IO ()
testWriteNoPath = do
    -- Set up the mock debugger
    newDB <- U.newDebugger Nothing Nothing
    let mockDB = Mc.mockCommandEdit "write" newDB
    -- Test execution as SimpleIO
    resultDB <- Mc.routeCommandModeAsSimpleIO mockDB
    -- Check the results
    U.checkCommandEdit [] resultDB
    T.message resultDB `shouldBe` "Save path required"

-- =============================================================== --
-- Testing the :save command

setupHelloWorld69 :: IO T.Debugger
-- ^Load the HelloWorld.bf test script, set a break point at BF
-- statement 69 and jump execution to that break point.
setupHelloWorld69 = U.newDebugger (Just "HelloWorld.bf") Nothing
                    >>= U.nextDebugger D.moveCursorRight 69
                    >>= pure . D.jumpForward . D.setBreakPoint

testSaveDefaultPath :: IO ()
testSaveDefaultPath = do
    -- Setup the mock debugger
    mockDB <- Mc.mockCommandEdit "save" <$> setupHelloWorld69
    let Just path = D.toDebugPath <$> T.scriptPath mockDB
    -- Test execution as SimpleIO
    resultDB <- Mc.routeCommandModeAsSimpleIO mockDB
    -- Check the results
    T.message resultDB `shouldBe` "State saved to " ++ path
    U.checkCommandEdit [] resultDB
    resultIO   <- BS.readFile path
    expectedIO <- BS.readFile . U.getTestPath $ "TestSaveDefaultPath.defug"
    resultIO `shouldBe` expectedIO

testSaveNewPath :: IO ()
testSaveNewPath = do
    -- Setup the mock debugger
    newDB <- setupHelloWorld69
    let path         = U.getTempTestPath "TestSaveNewPath.defug"
        mockDB       = Mc.mockCommandEdit ("save " <> Tx.pack path) newDB
        Just badPath = D.toDebugPath <$> T.scriptPath mockDB
    -- Test execution as SimpleIO
    resultDB <- Mc.routeCommandModeAsSimpleIO mockDB
    -- Check the results
    T.message resultDB `shouldBe` "State saved to " ++ path
    U.checkCommandEdit [] resultDB
    resultIO   <- BS.readFile path
    expectedIO <- BS.readFile . U.getTestPath $ "TestSaveDefaultPath.defug"
    resultIO `shouldBe` expectedIO
    doesPathExist badPath `shouldReturn` False

testSaveNoPath :: IO ()
testSaveNoPath = do
    -- Set up the mock debugger
    newDB <- U.newDebugger Nothing Nothing
    let mockDB = Mc.mockCommandEdit "save" newDB
    -- Test execution as SimpleIO
    resultDB <- Mc.routeCommandModeAsSimpleIO mockDB
    -- Check the results
    U.checkCommandEdit [] resultDB
    T.message resultDB `shouldBe` "Save path required"

