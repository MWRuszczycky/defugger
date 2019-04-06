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
import qualified Data.Vector     as V
import Brick.Widgets.Edit               ( editor        )
import Control.Monad.Except             ( throwError
                                        , liftEither    )
import Model.Parser                     ( parseDebug    )
import Data.Text                        ( Text          )
import Model.CoreIO                     ( tryReadFile
                                        , tryReadBytes  )

---------------------------------------------------------------------
-- Options and terminal initialization handling

getScript :: T.DefuggerOptions -> T.ErrorIO Text
getScript opts = case T.args opts of
                      []    -> throwError "A script file is required"
                      (x:_) -> tryReadFile x

getInput :: T.DefuggerOptions -> T.ErrorIO BS.ByteString
getInput opts = case T.args opts of
                     (_:x:_) -> tryReadBytes x
                     _       -> pure BS.empty

getDict :: T.DefuggerOptions -> T.ErrorIO T.Dictionary
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

initDebugger :: T.DefuggerOptions -> (Int, Int) -> T.ErrorIO T.Debugger
initDebugger opts (width,height) = do
    s <- getScript opts
    d <- getDict   opts
    x <- getInput  opts
    p <- liftEither . parseDebug d $ s
    pure T.Debugger { T.computer    = initComputer x
                    , T.dictionary  = d
                    , T.program     = p
                    , T.mode        = T.NormalMode
                    , T.wgtFocus    = T.ProgramWgt
                    , T.history     = [0]
                    , T.cursor      = 0
                    , T.breaks      = [ 0, V.length p - 1 ]
                    , T.readBackup  = []
                    , T.termWidth   = width
                    , T.termHeight  = height
                    , T.progWidth   = 30
                    , T.inFormat    = T.Asc
                    , T.outFormat   = T.Asc
                    , T.memView     = (0, height - 4)
                    , T.progView    = (0, height - 4)
                    , T.commandEdit = editor T.CommandWgt (Just 1) ""
                    , T.message     = "Welcome to the Defugger!"
                    }

---------------------------------------------------------------------
-- Computer initialization and resetting

initComputer :: BS.ByteString -> T.Computer
initComputer b = T.Computer { T.input  = b
                            , T.output = BS.empty
                            , T.memory = T.Tape [] 0 []
                            }
