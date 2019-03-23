{-# LANGUAGE OverloadedStrings #-}

module Loader
    ( initComputer
    , initDebugger
    , getScript
    , getDict
    , getInput
    ) where

import qualified Data.ByteString as BS
import qualified Model.Types     as T
import Control.Monad.Except             ( ExceptT
                                        , throwError
                                        , liftEither    )
import Model.Parser                     ( parse         )
import Model.CoreIO                     ( tryReadFile   )
import Data.Text                        ( Text          )

---------------------------------------------------------------------
-- Options handling

getScript :: T.DefuggerOptions -> ExceptT T.ErrString IO Text
getScript opts = case T.args opts of
                      []    -> throwError "A script file is required"
                      (x:_) -> tryReadFile x

getInput :: T.DefuggerOptions -> ExceptT T.ErrString IO BS.ByteString
getInput _ = pure BS.empty

getDict :: T.DefuggerOptions -> ExceptT T.ErrString IO T.Dictionary
getDict _ = pure bfDict

bfDict :: T.Dictionary
bfDict = T.toDictionary [ ( T.BFGT,    [">"] )
                        , ( T.BFLT,    ["<"] )
                        , ( T.BFPlus,  ["+"] )
                        , ( T.BFMinus, ["-"] )
                        , ( T.BFDot,   ["."] )
                        , ( T.BFComma, [","] )
                        , ( T.BFStart, ["["] )
                        , ( T.BFStop,  ["]"] )
                        , ( T.BFHash,  ["#"] )
                        ]

---------------------------------------------------------------------
-- Debuggeer initialization and resetting

initDebugger :: T.DefuggerOptions -> ExceptT T.ErrString IO T.Debugger
initDebugger opts = do
    s <- getScript opts
    d <- getDict   opts
    x <- getInput  opts
    p <- liftEither . parse d $ s
    pure T.Debugger { T.computer   = initComputer x
                    , T.dictionary = d
                    , T.program    = p
                    , T.status     = T.Normal
                    }

---------------------------------------------------------------------
-- Computer initialization and resetting

initComputer :: BS.ByteString -> T.Computer
initComputer b = T.Computer { T.input  = b
                            , T.output = BS.empty
                            , T.memory = T.Tape [] 0 []
                            }
