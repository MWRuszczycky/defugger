{-# LANGUAGE OverloadedStrings #-}

module ModelTest.DebuggerTest.StateTests
    ( spec
    ) where

import qualified Data.ByteString         as BS
import qualified Data.Text.IO            as Tx
import qualified UtilitiesTest           as U
import qualified Model.Types             as T
import qualified Model.Debugger.Debugger as D
import qualified Model.Parser            as M
import Data.Foldable                            ( toList        )
import Data.Default                             ( def           )
import Test.Hspec                               ( Spec
                                                , describe
                                                , shouldBe
                                                , shouldSatisfy
                                                , it            )

spec :: Spec
spec = do
    describe "Deserializing a bytestring as debugger/computer state" $ do
        it "Deserializes TestSave101.defug (HelloWorld.bf at stmt 59)"
            testDecodeDebugger101
        it "Deserializes TestSave102.defug (empty program at stmt 0)"
            testDecodeDebugger102
        it "Deserializes TestSave103.defug (WriteHelloWorld at stmt 8)"
            testDecodeDebugger103
        it "Deserializes TestSave104.defug (HelloWorld.bf fully evaluated)"
            testDecodeDebugger104
        it "Correctly fails to deserialize HelloWorld.bf"
            testDecodeDebugger201

-- =============================================================== --

testDecodeDebugger101 :: IO ()
-- ^See also ControllerTest.CommandBindingsTest.testSave101
testDecodeDebugger101 = do
    let testPath   = U.getTestPath $ "TestSave101.defug"
        resultPath = U.getTestPath $ "HelloWorld.bf"
    -- The input files are dummies and the loaded data should be replaced.
    newDb       <- U.newDebugger (Just "HelloWorld.bf") (Just "HelloWorld.out")
    bs          <- BS.readFile testPath
    Right (_,p) <- M.parseDebug def <$> Tx.readFile resultPath
    case D.decodeDebugger newDb bs of
         Left err -> error $ "decodeComputer fails to decode: " <> err
         Right db -> do
             let c = T.computer db
             D.getPosition db       `shouldBe`      69
             T.program db           `shouldBe`      p
             (show . T.memory) c    `shouldBe`      "0 0 72 [111] 88 32 8"
             T.output c             `shouldBe`      "Hell"
             T.input  c             `shouldSatisfy` BS.null
             T.initialInput db      `shouldSatisfy` BS.null
             (toList . T.breaks) db `shouldBe`      [0, 69, 107]
             T.scriptPath db        `shouldBe`      Nothing
             T.inputPath  db        `shouldBe`      Nothing

testDecodeDebugger102 :: IO ()
-- ^Decoding computer state where there is no program.
testDecodeDebugger102 = do
    let testPath = U.getTestPath $ "TestSave102.defug"
    -- The input files are dummies and the loaded data should be replaced.
    newDb <- U.newDebugger (Just "HelloWorld.bf") (Just "HelloWorld.out")
    bs    <- BS.readFile testPath
    case D.decodeDebugger newDb bs of
         Left err -> error $ "decodeComputer fails to decode: " <> err
         Right db -> do
             let c = T.computer db
             D.getPosition db        `shouldBe`      0
             (toList . T.program) db `shouldBe`      [T.DBStart, T.DBEnd]
             (show . T.memory) c     `shouldBe`      "[0]"
             T.output c              `shouldSatisfy` BS.null
             T.input  c              `shouldSatisfy` BS.null
             T.initialInput db       `shouldSatisfy` BS.null
             (toList . T.breaks) db  `shouldBe`      [0, 1]
             T.scriptPath db         `shouldBe`      Nothing
             T.inputPath  db         `shouldBe`      Nothing

testDecodeDebugger103 :: IO ()
-- ^Test deserialization with remaining input (WriteHelloWorld, position)
testDecodeDebugger103 = do
    let testPath   = U.getTestPath $ "TestSave103.defug"
        resultPath = U.getTestPath $ "WriteHelloWorld.bf"
    -- The input files are dummies and the loaded data should be replaced.
    newDb       <- U.newDebugger (Just "HelloWorld.bf") (Just "HelloWorld.out")
    bs          <- BS.readFile testPath
    Right (_,p) <- M.parseDebug def <$> Tx.readFile resultPath
    case D.decodeDebugger newDb bs of
         Left err -> error $ "decodeComputer fails to decode: " <> err
         Right db -> do
             let c = T.computer db
             D.getPosition db       `shouldBe`      8
             T.program db           `shouldBe`      p
             (show . T.memory) c    `shouldBe`      "72 101 108 108 [0]"
             T.output c             `shouldSatisfy` BS.null
             T.input  c             `shouldBe`      "o World!\n"
             T.initialInput db      `shouldBe`      "Hello World!\n"
             (toList . T.breaks) db `shouldBe`      [0, 8, 63]
             T.scriptPath db        `shouldBe`      Nothing
             T.inputPath  db        `shouldBe`      Nothing

testDecodeDebugger104 :: IO ()
-- ^Test deserialization with fully evaluated HelloWorld.
testDecodeDebugger104 = do
    let testPath   = U.getTestPath $ "TestSave104.defug"
        resultPath = U.getTestPath $ "HelloWorld.bf"
    -- The input files are dummies and the loaded data should be replaced.
    newDb       <- U.newDebugger (Just "HelloWorld.bf") (Just "HelloWorld.out")
    bs          <- BS.readFile testPath
    Right (_,p) <- M.parseDebug def <$> Tx.readFile resultPath
    case D.decodeDebugger newDb bs of
         Left err -> error $ "decodeComputer fails to decode: " <> err
         Right db -> do
             let c = T.computer db
             D.getPosition db       `shouldBe`      107
             T.program db           `shouldBe`      p
             (show . T.memory) c    `shouldBe`      "0 0 72 100 87 33 [10]"
             T.output c             `shouldBe`      "Hello World!\n"
             T.input  c             `shouldSatisfy` BS.null
             T.initialInput db      `shouldSatisfy` BS.null
             (toList . T.breaks) db `shouldBe`      [0, 107]
             T.scriptPath db        `shouldBe`      Nothing
             T.inputPath  db        `shouldBe`      Nothing

testDecodeDebugger201 :: IO ()
-- ^Test for failure to deserialize something which should not.
testDecodeDebugger201 = do
    let testPath = U.getTestPath $ "HelloWorld.bf"
    -- The input files are dummies and the loaded data should be replaced.
    newDb <- U.newDebugger (Just "HelloWorld.bf") (Just "HelloWorld.out")
    bs    <- BS.readFile testPath
    case D.decodeDebugger newDb bs of
         Left _  -> pure ()
         Right _ -> error $ "Deserializes HelloWorld.bf without error."
