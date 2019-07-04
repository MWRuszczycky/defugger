{-# LANGUAGE OverloadedStrings #-}

module Controller.Settings
    ( settings
    , parseSet
    , parseUnset
    ) where

-- =============================================================== --
-- Debugger settings that can be manipulated using the <set> and   --
-- <unset> commands                                                --
-- =============================================================== --

import qualified Data.Sequence           as Seq
import qualified Data.Text               as Tx
import qualified Model.Types             as T
import qualified Model.Debugger.Debugger as D
import Control.Monad                            ( guard          )
import Text.Read                                ( readMaybe      )
import Data.List                                ( find           )
import Data.Text                                ( Text           )

-- =============================================================== --
-- Settings hub and router

parseSet :: [String] -> Either T.ErrString (T.Debugger -> T.Debugger)
parseSet []     = Left "Nothing to set"
parseSet (x:xs) = maybe err go . find ( (==x) . T.settingName ) $ settings
    where err  = Left $ "Cannot set: " ++ x ++ ", so such setting"
          go s = T.setting s $ xs

parseUnset :: [String] -> Either T.ErrString (T.Debugger -> T.Debugger)
parseUnset []     = Left "Nothing to unset"
parseUnset (x:xs) = maybe err go . find ( (==x) . T.settingName ) $ settings
    where err  = Left $ "Cannot unset: " ++ x ++ ", no such setting"
          go s = T.unsetting s $ xs

settings :: [T.Setting]
settings = [ T.Setting "hex"     setHex     unsetHex     setHexHelp
           , T.Setting "dec"     setDec     unsetDec     setDecHelp
           , T.Setting "ascii"   setAsc     unsetAsc     setAscHelp
           , T.Setting "break"   setBreak   unsetBreak   setBreakHelp
           , T.Setting "history" setHistory unsetHistory setHistoryHelp
           , T.Setting "width"   setWidth   unsetWidth   setWidthHelp
           ]

-- =============================================================== --
-- Settings hub and router

---------------------------------------------------------------------
-- local type abbreviation

type SettingFunction = [String] -> Either T.ErrString (T.Debugger -> T.Debugger)

-- hex --------------------------------------------------------------

setHexHelp :: Text
setHexHelp = "Set byte display format in the current window to hexdecimal.\n"

setHex, unsetHex :: SettingFunction
setHex _   = pure $ D.noMessage . D.changeFormat T.Hex
unsetHex _ = pure D.noMessage

-- dec --------------------------------------------------------------

setDecHelp :: Text
setDecHelp = "Set the byte display format in the current window to decimal.\n"

setDec, unsetDec :: SettingFunction
setDec _   = pure $ D.noMessage . D.changeFormat T.Dec
unsetDec _ = pure D.noMessage

-- ascii ------------------------------------------------------------

setAscHelp :: Text
setAscHelp = "Set the byte display format in the current window to ascii.\n"

setAsc, unsetAsc :: SettingFunction
setAsc _   = pure $ D.noMessage . D.changeFormat T.Asc
unsetAsc _ = pure D.noMessage

-- break ------------------------------------------------------------

setBreakHelp :: Text
setBreakHelp = Tx.unlines hs
    where hs = [ "Insert or remove a break point at the cursor position."
               , "To remove all break points, use:\n"
               , "  unset break all"
               ]

setBreak :: SettingFunction
setBreak _ = pure $ D.noMessage . D.setBreakPoint

unsetBreak :: SettingFunction
unsetBreak ("all":_) = pure $ D.noMessage . D.unsetAllBreakPoints
unsetBreak _         = pure $ D.noMessage . D.unsetBreakPoint

-- history ----------------------------------------------------------

setHistoryHelp :: Text
setHistoryHelp = Tx.unlines hs
    where hs = [ "The number of individual debug steps to save in history for"
               , "reversion (i.e., stepping or jumping back). For example,\n"
               , "  set history 5000\n"
               , "ensures that you will be able to revert or 'undo' the last"
               , "5000 BF statements executed. Larger histories allow you to"
               , "to step back further in the program; however, they will also"
               , "occupy more memory. The default history size is 1000. The"
               , "history size should not affect affect the execution speed of"
               , "the BF program in debugger mode. Note that you can also"
               , "revert the entire program by using the <reset> command."
               ]

setHistory :: SettingFunction
setHistory []    = pure D.noMessage
setHistory (x:_) = maybe err pure . go $ x
    where err  = Left $ "Cannot set history reversion depth to " ++ x
          go y = do n <- readMaybe y
                    guard (n >= 0 )
                    pure $ \ db -> let h = T.history db
                                   in  db { T.message   = ""
                                          , T.histDepth = n + 1
                                          , T.history   = Seq.take (n+1) h
                                          }

unsetHistory :: SettingFunction
unsetHistory _ = pure D.noMessage

-- width ------------------------------------------------------------

setWidthHelp :: Text
setWidthHelp = Tx.unlines hs
    where hs = [ "The number of characters to display per line in the program"
               , "window. The minimum character width is 10." ]

setWidth :: SettingFunction
setWidth []    = Left "A value for the new program width must be supplied"
setWidth (x:_) = maybe err (pure . go) . readMaybe $ x
    where err  = Left $ "Cannot set width to " ++ x
          go n | n < 10    = \ db -> db { T.message = "Invalid width" }
               | otherwise = \ db -> db { T.message = "", T.progWidth = n }

unsetWidth :: SettingFunction
unsetWidth _ = pure D.noMessage

---------------------------------------------------------------------
