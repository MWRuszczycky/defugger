{-# LANGUAGE OverloadedStrings #-}

module Controller
    ( execute
    , getScript
    , formatOutput
    ) where

import qualified Data.ByteString as B
import qualified Data.Text.IO    as Tx
import qualified Types           as T
import System.Directory                 ( doesFileExist )
import Data.Text                        ( Text       )
import Compiler                         ( runProgram )
import Parser                           ( parse      )

execute :: B.ByteString -> Text -> Either T.ErrString T.Computer
execute b t = parse bfDict t >>= runProgram ( initComputer b )

getScript :: [String] -> IO (Either T.ErrString Text)
getScript []    = pure $ Left "A script file is required"
getScript (x:_) = do
    exists <- doesFileExist x
    if exists
       then Right <$> Tx.readFile x
       else pure . Left $ "Cannot find script file " ++ x

formatOutput :: B.ByteString -> String
formatOutput = map ( toEnum . fromIntegral ) . B.unpack

initComputer :: B.ByteString -> T.Computer
initComputer b = T.Computer { T.input  = b
                            , T.output = B.empty
                            , T.memory = T.Tape [] 0 []
                            }

bfDict :: T.Dictionary
bfDict = T.dictionary [ ( T.BFGT,    [">"] )
                      , ( T.BFLT,    ["<"] )
                      , ( T.BFPlus,  ["+"] )
                      , ( T.BFMinus, ["-"] )
                      , ( T.BFDot,   ["."] )
                      , ( T.BFComma, [","] )
                      , ( T.BFStart, ["["] )
                      , ( T.BFStop,  ["]"] )
                      , ( T.BFHash,  ["#"] )
                      ]
