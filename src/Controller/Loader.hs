{-# LANGUAGE OverloadedStrings #-}

module Controller.Loader
    ( bfDict
    , initComputer
    , initDebugger
    , resetDebugger
    , getScriptPath
    , getInputPath
    , getDict
    ) where

import qualified Data.ByteString as BS
import qualified Model.Types     as T
import qualified Data.Vector     as V
import qualified Data.Set        as Set
import qualified Data.Text       as Tx
import Brick.Widgets.Edit               ( editor        )
import Control.Monad.Except             ( liftEither    )
import Model.Parser                     ( parseDebug    )
import Model.CoreIO                     ( tryReadFile
                                        , tryReadBytes  )

---------------------------------------------------------------------
-- Options and terminal initialization handling

getScriptPath :: T.DefuggerOptions -> Maybe FilePath
getScriptPath opts = case T.args opts of
                          (x:_) -> Just x
                          _     -> Nothing

getInputPath :: T.DefuggerOptions -> Maybe FilePath
getInputPath opts = case T.args opts of
                         (_:x:_) -> Just x
                         _       -> Nothing

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
    let scriptPath = getScriptPath opts
    s  <- maybe (pure Tx.empty) tryReadFile scriptPath
    x  <- maybe (pure BS.empty) tryReadBytes . getInputPath  $ opts
    d  <- getDict opts
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
                    , T.scriptPath  = scriptPath
                    }

resetDebugger :: Maybe FilePath -> Maybe FilePath -> T.Debugger
                 -> T.ErrorIO T.Debugger
-- ^Given an already initialized debugger, reset the computer,
-- program, and input keeping everything else the same.
resetDebugger scriptPath inputPath db = do
    s <- maybe (pure Tx.empty) tryReadFile  scriptPath
    x <- maybe (pure BS.empty) tryReadBytes inputPath
    p <- liftEither . parseDebug (T.dictionary db) $ s
    pure db { -- Core model
              T.computer    = initComputer x
            , T.program     = p
              -- Positioning, running mode and history
            , T.cursor      = 0
            , T.history     = [0]
            , T.readBackup  = []
              -- Settings
            , T.breaks      = Set.fromList [ 0, V.length p - 1 ]
            , T.scriptPath  = scriptPath
            }

---------------------------------------------------------------------
-- Computer initialization and resetting

initComputer :: BS.ByteString -> T.Computer
initComputer b = T.Computer { T.input  = b
                            , T.output = BS.empty
                            , T.memory = T.Tape [] 0 []
                            }
