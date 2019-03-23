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
import Control.Monad.Except             ( runExceptT )
import Data.Text                        ( Text       )
import Model.Parser                     ( parse      )
import Test.Hspec                       ( Spec (..)
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

scriptNoInput :: FilePath -> FilePath -> IO ()
scriptNoInput s t = do
    opts <- pure [s] >>= SU.getOptions
    case T.mode opts of
         T.Interpreter -> runExceptT (SU.interpreter opts) >>= checkResult t
         _             -> error "Test failed: Interpreter mode expected"

checkResult :: FilePath -> Either T.ErrString T.Computer -> IO ()
checkResult _ (Left err) = error $ "Test failed: " ++ err
checkResult t (Right c)  = do
    let output = SU.formatOutput . T.output $ c
    expected <- readFile t
    output `shouldBe` expected
