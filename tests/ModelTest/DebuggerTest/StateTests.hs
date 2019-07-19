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
            testDecodeComputer101
        it "Deserializes TestSave102.defug (empty program at stmt 0)"
            testDecodeComputer102
        it "Deserializes TestSave103.defug (WriteHelloWorld at stmt 8)"
            testDecodeComputer103
        it "Correctly fails to deserialize HelloWorld.bf"
            testDecodeComputer201

-- =============================================================== --

testDecodeComputer101 :: IO ()
-- ^See also ControllerTest.CommandBindingsTest.testSave101
testDecodeComputer101 = do
    let testPath   = U.getTestPath $ "TestSave101.defug"
        resultPath = U.getTestPath $ "HelloWorld.bf"
    bs       <- BS.readFile testPath
    Right hw <- M.parseDebug def <$> Tx.readFile resultPath
    case D.decodeComputer bs of
         Left err      -> error $ "decodeComputer fails to decode: " <> err
         Right (c,p,x) -> do
             x                   `shouldBe`      69
             p                   `shouldBe`      hw
             (show . T.memory) c `shouldBe`      "0 0 72 [111] 88 32 8"
             T.output c          `shouldBe`      "Hell"
             T.input  c          `shouldSatisfy` BS.null

testDecodeComputer102 :: IO ()
-- ^Decoding computer state where there is no program.
testDecodeComputer102 = do
    let testPath   = U.getTestPath $ "TestSave102.defug"
    bs <- BS.readFile testPath
    case D.decodeComputer bs of
         Left err      -> error $ "decodeComputer fails to decode: " <> err
         Right (c,p,x) -> do
             x                   `shouldBe`      0
             toList p            `shouldBe`      [T.DBStart, T.DBEnd]
             (show . T.memory) c `shouldBe`      "[0]"
             T.output c          `shouldSatisfy` BS.null
             T.input  c          `shouldSatisfy` BS.null

testDecodeComputer103 :: IO ()
-- ^Test deserialization with remaining input (WriteHelloWorld, position)
testDecodeComputer103 = do
    let testPath   = U.getTestPath $ "TestSave103.defug"
        resultPath = U.getTestPath $ "WriteHelloWorld.bf"
    bs       <- BS.readFile testPath
    Right hw <- M.parseDebug def <$> Tx.readFile resultPath
    case D.decodeComputer bs of
         Left err      -> error $ "decodeComputer fails to decode: " <> err
         Right (c,p,x) -> do
             x                   `shouldBe`      8
             p                   `shouldBe`      hw
             (show . T.memory) c `shouldBe`      "72 101 108 108 [0]"
             T.output c          `shouldSatisfy` BS.null
             T.input  c          `shouldBe`      "o World!\n"

testDecodeComputer201 :: IO ()
-- ^Test for failure to deserialize something which should not.
testDecodeComputer201 = do
    let testPath = U.getTestPath $ "HelloWorld.bf"
    bs <- BS.readFile testPath
    case D.decodeComputer bs of
         Left _  -> pure ()
         Right _ -> error $ "Deserializes HelloWord.bf without error."
