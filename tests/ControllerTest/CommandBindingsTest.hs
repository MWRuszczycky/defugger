{-# LANGUAGE OverloadedStrings #-}

module ControllerTest.CommandBindingsTest
    ( spec
    ) where

import qualified Model.Types               as T
import qualified Model.Utilities           as MU
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
        it "Writes a script correctly with default path (testWrite101)"
            testWrite101
        it "Writes a script with a newly supplied path (testWrite201)"
            testWrite201
        it "Does not write a script without a valid path (testWrite301)"
            testWrite301
    describe ":save command" $ do
        it "Serializing debugger state with default path (testSave101)"
            testSave101
        it "Serializing debugger state with new path (testSave201)"
            testSave201
        it "Does not save debugger state without a valid path (testSave301)"
            testSave301

-- =============================================================== --
-- Testing the :write command

testWrite101 :: IO ()
-- ^Writes HelloWorld.bf script with default path.
testWrite101 = do
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
    expectedIO <- Tx.readFile . U.getTestPath $ "TestWrite101.bf"
    resultIO `shouldBe` expectedIO

testWrite201 :: IO ()
-- ^Writes HelloWorld.bf script with a new path.
testWrite201 = do
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
    expectedIO <- Tx.readFile . U.getTestPath $ "TestWrite101.bf"
    resultIO `shouldBe` expectedIO
    doesPathExist oldPath `shouldReturn` False

testWrite301 :: IO ()
-- ^Fails to write without a valid path. Should generate an error.
testWrite301 = do
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

testSave101 :: IO ()
-- ^Default path with HelloWorld.bf evaluated to position 69.
testSave101 = do
    -- Setup the mock debugger
    mockDB <- Mc.mockCommandEdit "save" <$> setupHelloWorld69
    let Just path = MU.toDebugPath <$> T.scriptPath mockDB
    -- Test execution as SimpleIO
    resultDB <- Mc.routeCommandModeAsSimpleIO mockDB
    -- Check the results
    T.message resultDB `shouldBe` "State saved to " ++ path
    U.checkCommandEdit [] resultDB
    resultIO   <- BS.readFile path
    expectedIO <- BS.readFile . U.getTestPath $ "TestSave101.defug"
    resultIO `shouldBe` expectedIO

testSave201 :: IO ()
-- ^New path with HelloWorld.bf evaluated to position 69.
testSave201 = do
    -- Setup the mock debugger
    newDB <- setupHelloWorld69
    let path         = U.getTempTestPath "TestSave201NewPath.defug"
        mockDB       = Mc.mockCommandEdit ("save " <> Tx.pack path) newDB
        Just badPath = MU.toDebugPath <$> T.scriptPath mockDB
    -- Test execution as SimpleIO
    resultDB <- Mc.routeCommandModeAsSimpleIO mockDB
    -- Check the results
    T.message resultDB `shouldBe` "State saved to " ++ path
    U.checkCommandEdit [] resultDB
    resultIO   <- BS.readFile path
    expectedIO <- BS.readFile . U.getTestPath $ "TestSave101.defug"
    resultIO `shouldBe` expectedIO
    doesPathExist badPath `shouldReturn` False

testSave301 :: IO ()
-- ^No valid path to save to. Should generate an error.
testSave301 = do
    -- Set up the mock debugger
    newDB <- U.newDebugger Nothing Nothing
    let mockDB = Mc.mockCommandEdit "save" newDB
    -- Test execution as SimpleIO
    resultDB <- Mc.routeCommandModeAsSimpleIO mockDB
    -- Check the results
    U.checkCommandEdit [] resultDB
    T.message resultDB `shouldBe` "Save path required"

