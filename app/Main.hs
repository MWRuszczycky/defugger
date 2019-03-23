{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Model.Types as T
import Control.Monad.Except        ( runExceptT      )
import System.Environment          ( getArgs         )
import StartUp                     ( getOptions
                                   , interpreter
                                   , debugger
                                   , endInterpreter
                                   , endDebugger     )

main :: IO ()
main = do
    opts <- getArgs >>= getOptions
    case T.mode opts of
         T.Interpreter -> runExceptT (interpreter opts) >>= endInterpreter
         T.DebugMode   -> runExceptT (debugger opts)    >>= endDebugger
         T.OptsError e -> putStrLn $ "Error: " ++ e
