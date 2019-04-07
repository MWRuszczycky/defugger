{-# LANGUAGE OverloadedStrings #-}

-- Do not add a module declaration or it will fail to compile

-- =============================================================== --
-- This is the bfprogram-test test-suite for testing BF programs
-- executed with the interepreter
-- =============================================================== --

import qualified Data.Text          as Tx
import qualified Data.ByteString    as B
import qualified Model.Types        as T
import qualified Model.Parser       as P
import qualified Controller.Router  as C
import qualified Controller.Loader  as L
import qualified StartUp            as SU
import Control.Monad.Except                 ( runExceptT )
import Data.Text                            ( Text       )
import Model.Parser                         ( parse      )
import Test.Hspec                           ( Spec (..)
                                            , describe
                                            , hspec
                                            , it
                                            , shouldBe   )

main :: IO ()
main = hspec $ do
    describe "Running BF programs with the default dictionary" $ do
        it "Correctly runs HelloWorld.bf" $ do
            scriptNoInput "tests/files/HelloWorld.bf"
                          "tests/files/HelloWorld.out"
        it "Correctly runs WriteHelloWorld.bf (input test)" $ do
            scriptWithInput "tests/files/WriteHelloWorld.bf"
                            "tests/files/WriteHelloWorld.txt"
                            "tests/files/HelloWorld.out"

scriptNoInput :: FilePath -> FilePath -> IO ()
scriptNoInput s t = do
    opts <- pure ["--run", s] >>= SU.getOptions
    case T.runMode opts of
         T.RunInterpreter -> runExceptT (SU.interpreter opts) >>= checkResult t
         _                -> error "Test failed: RunInterpreter mode expected"

scriptWithInput :: FilePath -> FilePath -> FilePath -> IO ()
scriptWithInput s i t = do
    opts <- pure ["--run", s, i] >>= SU.getOptions
    case T.runMode opts of
         T.RunInterpreter -> runExceptT (SU.interpreter opts) >>= checkResult t
         _                -> error "Test failed: RunInterpreter mode expected"

checkResult :: FilePath -> Either T.ErrString T.Computer -> IO ()
checkResult _ (Left err) = error $ "Test failed: " ++ err
checkResult t (Right c)  = do
    let output = SU.formatOutput . T.output $ c
    expected <- readFile t
    output `shouldBe` expected
