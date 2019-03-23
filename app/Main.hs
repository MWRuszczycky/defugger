{-# LANGUAGE OverloadedStrings #-}

module Main where

import System.Environment ( getArgs   )
import Loader             ( initCatfk )
import StartUp            ( runCatfk  )

main :: IO ()
main = getArgs >>= initCatfk >>= runCatfk
