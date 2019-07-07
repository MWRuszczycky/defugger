{-# LANGUAGE OverloadedStrings #-}
module ModelTest.InterpreterTests
    ( spec
    ) where

-- =============================================================== --
-- Tests for executing scripts by the interpreter.                 --
-- =============================================================== --

import qualified Model.Types        as T
import qualified StartUp            as SU
import qualified UtilitiesTest      as U
import Control.Monad.Except                 ( runExceptT )
import Test.Hspec                           ( Spec
                                            , describe
                                            , it
                                            , shouldBe   )

spec :: Spec
spec = do
    describe "Running BF programs with the default dictionary" $ do
        it "Correctly runs HelloWorld.bf" $ do
            scriptNoInput ( U.getTestPath "HelloWorld.bf"  )
                          ( U.getTestPath "HelloWorld.out" )
        it "Correctly runs WriteHelloWorld.bf (input test)" $ do
            scriptWithInput ( U.getTestPath "WriteHelloWorld.bf"  )
                            ( U.getTestPath "WriteHelloWorld.txt" )
                            ( U.getTestPath "HelloWorld.out"      )

scriptNoInput :: FilePath -> FilePath -> IO ()
scriptNoInput s t = runExceptT (args >>= SU.parseOptions) >>= either err go
    where args    = pure ["--run", s]
          err e   = error $ "Test failed: Unable to read options: " ++ e
          goError = error "Test failed: RunInterpreter mode expected"
          go opts = case T.runMode opts of
                         T.RunInterpreter -> runExceptT (SU.interpreter opts)
                                             >>= checkResult t
                         _                -> goError

scriptWithInput :: FilePath -> FilePath -> FilePath -> IO ()
scriptWithInput s i t = runExceptT (args >>= SU.parseOptions) >>= either err go
    where args    = pure ["--run", s, i]
          err e   = error $ "Test failed: Unable to read options: " ++ e
          goError = error "Test failed: RunInterpreter mode expected"
          go opts = case T.runMode opts of
                         T.RunInterpreter -> runExceptT (SU.interpreter opts)
                                             >>= checkResult t
                         _                -> goError

checkResult :: FilePath -> Either T.ErrString T.Computer -> IO ()
checkResult _ (Left err) = error $ "Test failed: " ++ err
checkResult t (Right c)  = do
    let output = SU.formatOutput . T.output $ c
    expected <- readFile t
    output `shouldBe` expected
