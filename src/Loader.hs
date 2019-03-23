{-# LANGUAGE OverloadedStrings #-}

module Loader
    ( initCatfk
    , initComputer
    , bfDict
    ) where

import qualified Data.ByteString as BS
import qualified Data.Text.IO    as Tx
import qualified Model.Types     as T
import Model.CoreIO                     ( tryReadFile   )
import Control.Monad.Except             ( runExceptT    )
import Data.Text                        ( Text          )

initCatfk :: [String] -> IO T.OuterState
initCatfk []    = pure . Left $ "A script file is required"
initCatfk (x:_) = runExceptT $ do
    s <- tryReadFile x
    pure T.CatfkState { T.computer   = initComputer BS.empty
                      , T.mode       = T.RunAndDone
                      , T.dictionary = bfDict
                      , T.script     = s
                      }

initComputer :: BS.ByteString -> T.Computer
initComputer b = T.Computer { T.input  = b
                            , T.output = BS.empty
                            , T.memory = T.Tape [] 0 []
                            }

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
