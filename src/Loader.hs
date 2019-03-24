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
import Data.List                        ( foldl'        )
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
                    , T.program    = toDebug p
                    , T.status     = T.Normal
                    , T.position   = 0
                    }

toDebug :: T.Program -> T.DBProgram
toDebug = (++[T.DBEnd]) . (T.DBStart:) . reverse . snd . foldl' go (1,[])
    where go x      (T.DoNothing  ) = x
          go (n,dp) (T.Increment  ) = (n+1, T.DBIncrement : dp)
          go (n,dp) (T.Decrement  ) = (n+1, T.DBDecrement : dp)
          go (n,dp) (T.Advance    ) = (n+1, T.DBAdvance   : dp)
          go (n,dp) (T.Backup     ) = (n+1, T.DBBackup    : dp)
          go (n,dp) (T.ReadIn     ) = (n+1, T.DBReadIn    : dp)
          go (n,dp) (T.WriteOut   ) = (n+1, T.DBWriteOut  : dp)
          go (n,dp) (T.WhileLoop p) = let (n',dq) = foldl' go (n+1,[]) p
                                          xs      = T.DBCloseLoop n : dq
                                          ys      = T.DBOpenLoop n' : dp
                                      in (n'+1, xs ++ ys)

---------------------------------------------------------------------
-- Computer initialization and resetting

initComputer :: BS.ByteString -> T.Computer
initComputer b = T.Computer { T.input  = b
                            , T.output = BS.empty
                            , T.memory = T.Tape [] 0 []
                            }
