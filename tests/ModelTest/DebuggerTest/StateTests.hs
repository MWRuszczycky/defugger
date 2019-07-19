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
import Data.Default                             ( def           )
import Test.Hspec                               ( Spec
                                                , describe
                                                , shouldBe
                                                , shouldSatisfy
                                                , it            )

spec :: Spec
spec = do
    describe "Deserializing a bytestring as debugger/computer" $ do
        it "Correctly deserializes TestSave101.defug (testDecodeComputer101)"
            testDecodeComputer101

-- =============================================================== --

testDecodeComputer101 :: IO ()
-- ^See also ControllerTest.CommandBindingsTest.testSaveDefaultPath
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
