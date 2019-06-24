{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Model.Types as T
import Control.Monad.Except        ( runExceptT
                                   , liftIO          )
import System.Environment          ( getArgs         )
import StartUp                     ( parseOptions
                                   , interpreter
                                   , debugger
                                   , endInterpreter
                                   , endDebugger     )

main :: IO ()
main = runExceptT ( liftIO getArgs >>= parseOptions ) >>= either err go
    where err e   = putStrLn $ "Error: " ++ e
          go opts = case T.runMode opts of
                         T.RunInterpreter -> runExceptT (interpreter opts)
                                             >>= endInterpreter
                         T.RunDebugger    -> runExceptT (debugger opts)
                                             >>= endDebugger
