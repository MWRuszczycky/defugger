{-# LANGUAGE OverloadedStrings #-}

module Main where

import System.Environment ( getArgs      )
import Loader             ( initDefugger )
import StartUp            ( runDefugger  )

main :: IO ()
main = getArgs >>= initDefugger >>= runDefugger
