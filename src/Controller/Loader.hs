{-# LANGUAGE OverloadedStrings #-}

module Controller.Loader
    ( bfDict
    , initComputer
    , initDebugger
    , getScript
    , getDict
    , getInput
    ) where

import qualified Data.ByteString as BS
import qualified Model.Types     as T
import qualified Data.Vector     as V
import qualified Data.Set        as Set
import qualified Data.Text       as Tx
import Brick.Widgets.Edit               ( editor        )
import Control.Monad.Except             ( liftEither
                                        , throwError    )
import Model.Parser                     ( parseDebug    )
import Data.Text                        ( Text          )
import Model.CoreIO                     ( tryReadFile
                                        , tryReadBytes  )

---------------------------------------------------------------------
-- Options and terminal initialization handling

getSavePath :: T.DefuggerOptions -> T.ErrorIO (Maybe FilePath)
getSavePath opts =
    case T.args opts of
         []    -> pure Nothing
         (x:_) -> pure . Just $ x

getScript :: T.DefuggerOptions -> T.ErrorIO Text
getScript opts = do
    mbFp <- getSavePath opts
    case (mbFp, T.runMode opts) of
         (Nothing, T.RunInterpreter) -> throwError "A BF script file is required."
         (Nothing, T.RunDebugger   ) -> pure Tx.empty
         (Just fp, _               ) -> tryReadFile fp

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
    fp <- getSavePath opts
    s  <- getScript opts
    d  <- getDict   opts
    x  <- getInput  opts
    p  <- liftEither . parseDebug d $ s
    pure T.Debugger { -- Core model
                      T.computer    = initComputer x
                    , T.dictionary  = d
                    , T.program     = p
                      -- Positioning, running mode and history
                    , T.mode        = T.NormalMode
                    , T.wgtFocus    = T.ProgramWgt
                    , T.cursor      = 0
                    , T.history     = [0]
                    , T.readBackup  = []
                      -- Command and status widgets
                    , T.commandEdit = editor T.CommandWgt (Just 1) ""
                    , T.message     = "Welcome to the Defugger: A BF Debugger!"
                      -- Terminal and display parameters
                    , T.termWidth   = width
                    , T.termHeight  = height
                    , T.progView    = (0, height - 5)
                    , T.memView     = (0, height - 5)
                      -- Settings
                    , T.breaks      = Set.fromList [ 0, V.length p - 1 ]
                    , T.progWidth   = 30
                    , T.inFormat    = T.Asc
                    , T.outFormat   = T.Asc
                    , T.savePath    = fp
                    }

---------------------------------------------------------------------
-- Computer initialization and resetting

initComputer :: BS.ByteString -> T.Computer
initComputer b = T.Computer { T.input  = b
                            , T.output = BS.empty
                            , T.memory = T.Tape [] 0 []
                            }
