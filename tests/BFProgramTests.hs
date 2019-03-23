{-# LANGUAGE OverloadedStrings #-}

-- Do not add a module declaration or it will fail to compile

-- =============================================================== --
-- This is the bfprogram-test test-suite for testing BF programs
-- executed with the interepreter
-- =============================================================== --

import qualified Data.Text       as Tx
import qualified Data.ByteString as B
import qualified Model.Types     as T
import qualified Model.Parser    as P
import qualified Controller      as C
import qualified Loader          as L
import qualified StartUp         as SU
import Data.Text                        ( Text      )
import Model.Parser                     ( parse     )
import Test.Hspec                       ( Spec (..)
                                        , describe
                                        , hspec
                                        , it
                                        , shouldBe  )

main :: IO ()
main = hspec $ do
    describe "Running BF programs with the default dictionary" $ do
        it "Correctly runs HelloWorld.bf" $ do
            scriptNoInput "tests/files/HelloWorld.bf"
                          "tests/files/HelloWorld.out"

scriptNoInput :: FilePath -> FilePath -> IO ()
scriptNoInput s t = do
    start    <- L.initCatfk =<< pure [s]
    result   <- pure $ C.runAndDone =<< start
    expected <- readFile t
    case result of
         Left e  -> error e
         Right x -> let output = SU.formatOutput . T.output . T.computer $ x
                    in  output `shouldBe` expected
