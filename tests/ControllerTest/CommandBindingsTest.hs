{-# LANGUAGE OverloadedStrings #-}

module ControllerTest.CommandBindingsTest
    ( spec
    ) where

import qualified Model.Types               as T
import qualified UtilitiesTest             as U
import qualified ControllerTest.MockRouter as Mc
import qualified Data.Text                 as Tx
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
    -- Set up mocked debugger
    newDB <- U.newDebugger (Just "tests/files/HelloWorld.bf") Nothing
    let path   = U.getTempTestPath "HelloWorld-temp.bf"
        mockDB = Mc.mockCommandEdit "write" $ newDB { T.scriptPath = Just path }
    -- Test execution as SimpleIO
    resultDB <- Mc.routeCommandModeAsSimpleIO mockDB
    -- Check the results
    T.message resultDB `shouldBe` "saved to " ++ path
    U.checkCommandEdit [] resultDB
    resultIO   <- Tx.readFile path
    expectedIO <- Tx.readFile "tests/files/TestWriteDefaultPath.bf"
    resultIO `shouldBe` expectedIO

testWriteNewPath :: IO ()
testWriteNewPath = do
    -- Set up the mocked debugger
    newDB <- U.newDebugger (Just "tests/files/HelloWorld.bf") Nothing
    let oldPath = U.getTempTestPath "HelloWorld-old.bf"
        newPath = U.getTempTestPath "HelloWorld-new.bf"
        mockDB  = Mc.mockCommandEdit ( "write " <> Tx.pack newPath ) $
                      newDB { T.scriptPath = Just oldPath }
    -- Test execution as SimpleIO
    resultDB <- Mc.routeCommandModeAsSimpleIO mockDB
    -- Check the results
    T.message resultDB `shouldBe` "saved to " ++ newPath
    U.checkCommandEdit [] resultDB
    resultIO   <- Tx.readFile newPath
    expectedIO <- Tx.readFile "tests/files/TestWriteDefaultPath.bf"
    resultIO `shouldBe` expectedIO
    doesPathExist oldPath `shouldReturn` False

testWriteNoPath :: IO ()
testWriteNoPath = do
    -- Set up the mocke debugger
    newDB <- U.newDebugger Nothing Nothing
    let mockDB = Mc.mockCommandEdit "write" newDB
    -- Test execution as SimpleIO
    resultDB <- Mc.routeCommandModeAsSimpleIO mockDB
    -- Check the results
    U.checkCommandEdit [] resultDB
    T.message resultDB `shouldBe` "Save path required"
