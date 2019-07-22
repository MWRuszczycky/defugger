{-# LANGUAGE OverloadedStrings #-}
module ModelTest.InterpreterTests
    ( spec
    ) where

-- =============================================================== --
-- Tests for executing scripts by the interpreter.                 --
-- =============================================================== --

import qualified Model.Types             as T
import qualified StartUp                 as SU
import qualified UtilitiesTest           as U
import qualified Data.Set                as Set
import qualified Data.ByteString         as BS
import qualified Model.Debugger.Debugger as D
import Control.Monad.Except                     ( runExceptT )
import Test.Hspec                               ( Spec
                                                , describe
                                                , it
                                                , shouldBe   )

type Result = (T.Computer, T.DBProgram, Set.Set Int, BS.ByteString)

spec :: Spec
spec = do
    describe "Running interpreter with the default dictionary" $ do
        it "Correctly runs HelloWorld.bf" $ do
            scriptNoInput ( U.getTestPath "HelloWorld.bf"     )
                          ( U.getTestPath "TestSave104.defug" )
        it "Correctly runs HelloWorldBreak.bf" $ do
            scriptNoInput ( U.getTestPath "BreakHelloWorld.bf" )
                          ( U.getTestPath "TestSave101.defug"  )
        it "Correctly runs WriteHelloWorld.bf (input test)" $ do
            scriptWithInput ( U.getTestPath "WriteHelloWorld.bf"  )
                            ( U.getTestPath "WriteHelloWorld.txt" )
                            ( U.getTestPath "TestSave105.defug"   )

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

checkResult :: FilePath -> Either T.ErrString Result -> IO ()
checkResult _  (Left  e ) = error $ "Test failed: " ++ e
checkResult fp (Right r ) = do
    expected <- BS.readFile fp
    D.encodeResult r `shouldBe` expected
