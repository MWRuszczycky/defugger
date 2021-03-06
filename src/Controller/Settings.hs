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
import Data.Text                                ( Text           )
import Control.Monad                            ( guard          )
import Text.Read                                ( readMaybe      )
import Data.List                                ( find           )

-- =============================================================== --
-- Settings hub and router

parseSet :: [Text] -> Either T.ErrString (T.Debugger -> T.Debugger)
parseSet []     = Left "Nothing to set"
parseSet (x:xs) = maybe err go . find ( (==x) . T.settingName ) $ settings
    where err  = Left $ "Cannot set: " ++ Tx.unpack x ++ ", so such setting"
          go s = T.setting s $ xs

parseUnset :: [Text] -> Either T.ErrString (T.Debugger -> T.Debugger)
parseUnset []     = Left "Nothing to unset"
parseUnset (x:xs) = maybe err go . find ( (==x) . T.settingName ) $ settings
    where err  = Left $ "Cannot unset: " ++ Tx.unpack x ++ ", no such setting"
          go s = T.unsetting s $ xs

settings :: [T.Setting]
settings = [ T.Setting "hex"     set_hex     unset_hex     hex_help
           , T.Setting "dec"     set_dec     unset_dec     dec_help
           , T.Setting "ascii"   set_asc     unset_asc     asc_help
           , T.Setting "break"   set_break   unset_break   break_help
           , T.Setting "history" set_history unset_history history_help
           , T.Setting "safe"    set_safe    unset_safe    safe_help
           , T.Setting "unsafe"  set_unsafe  unset_unsafe  unsafe_help
           , T.Setting "width"   set_width   unset_width   width_help
           ]

-- =============================================================== --
-- Settings hub and router

-- hex --------------------------------------------------------------

hex_help ::T.HelpInfo
hex_help = T.HelpInfo ns us sh lh
    where ns = [ "hex" ]
          us = ":set hex"
          sh = "Set byte display format in the current window to hexadecimal"
          lh = Tx.empty

set_hex, unset_hex :: T.SettingAction
set_hex _   = pure $ D.noMessage . D.changeFormat T.Hex
unset_hex _ = pure D.noMessage

-- dec --------------------------------------------------------------

dec_help :: T.HelpInfo
dec_help = T.HelpInfo ns us sh lh
    where ns = [ "dec" ]
          us = ":set dec"
          sh = "Set the byte display format in the current window to decimal"
          lh = Tx.empty

set_dec, unset_dec :: T.SettingAction
set_dec _   = pure $ D.noMessage . D.changeFormat T.Dec
unset_dec _ = pure D.noMessage

-- ascii ------------------------------------------------------------

asc_help :: T.HelpInfo
asc_help = T.HelpInfo ns us sh lh
    where ns = [ "ascii" ]
          us = ":set ascii"
          sh = "Set the byte display format in the current window to ascii"
          lh = Tx.empty

set_asc, unset_asc :: T.SettingAction
set_asc _   = pure $ D.noMessage . D.changeFormat T.Asc
unset_asc _ = pure D.noMessage

-- break ------------------------------------------------------------

break_help :: T.HelpInfo
break_help = T.HelpInfo ns us sh (Tx.unlines lh)
    where ns = [ "break" ]
          us = ":set break | :unset break [all]"
          sh = "Set or unset a break point at cursor"
          lh = [ "Insert or remove a break point at the cursor position."
               , "To remove all break points, use :unset break all"
               ]

set_break :: T.SettingAction
set_break _ = pure $ D.noMessage . D.setBreakPoint

unset_break :: T.SettingAction
unset_break ("all":_) = pure $ D.noMessage . D.unsetAllBreakPoints
unset_break _         = pure $ D.noMessage . D.unsetBreakPoint

-- history ----------------------------------------------------------

history_help :: T.HelpInfo
history_help = T.HelpInfo ns us sh (Tx.unlines lh)
    where ns = [ "history" ]
          us = ":set history DEPTH"
          sh = "Set the reversion history depth"
          lh = [ "The number of individual debug steps to save in history for"
               , "reversion (i.e., stepping or jumping back). For example,"
               , "  :set history 5000"
               , "ensures that you will be able to revert or 'undo' the last"
               , "5000 BF statements executed. Larger histories allow you to"
               , "to step back further in the program; however, they will also"
               , "occupy more memory. The default history size is 1000. The"
               , "history size should not affect affect the execution speed of"
               , "the BF program in debugger mode. Note that you can also"
               , "revert the entire program by using the <reset> command."
               ]

set_history :: T.SettingAction
set_history []    = pure D.noMessage
set_history (x:_) = maybe err pure . go . Tx.unpack $ x
    where err  = Left $ "Cannot set history reversion depth to " ++ Tx.unpack x
          go y = do n <- readMaybe y
                    guard (n >= 0 )
                    pure $ \ db -> let h = T.history db
                                   in  db { T.message   = ""
                                          , T.histDepth = n + 1
                                          , T.history   = Seq.take (n+1) h
                                          }

unset_history :: T.SettingAction
unset_history _ = pure D.noMessage

-- safe -------------------------------------------------------------

safe_help :: T.HelpInfo
safe_help = T.HelpInfo ns us sh (Tx.unlines lh)
    where ns =[ "safe" ]
          us = ":set safe | :unset safe"
          sh = "Forbid edits in a while-loop contianing the point of execution"
          lh = [ "This setting is just the opposite of <unsafe>. Thus,\n"
               , "  :set safe   = :unset unsafe"
               , "  :unset safe = :set unsafe\n"
               , "For more information, see :help unsafe"]

set_safe, unset_safe :: T.SettingAction
set_safe _   = pure $ \ db -> db { T.unsafeEdit = False }
unset_safe _ = pure $ \ db -> db { T.unsafeEdit = True  }

-- unsafe -----------------------------------------------------------

unsafe_help :: T.HelpInfo
unsafe_help = T.HelpInfo ns us sh (Tx.unlines lh)
    where ns = [ "unsafe" ]
          us = ":set unsafe | :unset unsafe"
          sh = "Allow edits in a while-loop containing the point of execution"
          lh = [ "When editing a BF program, you cannot add or delete BF"
               , "statements if they occur before the point of execution in"
               , "the script or if they are in the same outermost while-loop"
               , "(i.e., a [...] block) as the point of execution. This"
               , "ensures that the program state generated so far remains the"
               , "same after the edit. However, this can be overly restrictive,"
               , "especially when you know that program execution has not"
               , "advanced beyond where you want to apply the edit despite"
               , "being within the same while-loop. Therefore, you can toggle"
               , "this safety check using the <unsafe> setting making editing"
               , "of BF scripts easier. However, turing off this setting still"
               , "requires that the edit position be after the current point of"
               , "program execution in the script. Furthermore, there are no"
               , "longer any guarantees that the edits made will not affect the"
               , "state generated so far. You can also toggle the while-loop"
               , "safety check using the <safe> setting."
               ]

set_unsafe, unset_unsafe :: T.SettingAction
set_unsafe _   = pure $ \ db -> db { T.unsafeEdit = True  }
unset_unsafe _ = pure $ \ db -> db { T.unsafeEdit = False }

-- width ------------------------------------------------------------

width_help :: T.HelpInfo
width_help = T.HelpInfo ns us sh (Tx.unlines lh)
    where ns = [ "width" ]
          us = ":set width WIDTH"
          sh = "The number WIDTH of characters per line in the program window"
          lh = [ "The number of characters to display per line in the program"
               , "window. For example, to set the width to 50 characters,"
               , "  :set width 50"
               , "The minimum character width is 10."
               ]

set_width :: T.SettingAction
set_width []    = Left "A value for the new program width must be supplied"
set_width (x:_) = maybe err (pure . go) . readMaybe . Tx.unpack $ x
    where err  = Left $ "Cannot set width to " ++ Tx.unpack x
          go n | n < 10    = \ db -> db { T.message = "Invalid width" }
               | otherwise = \ db -> db { T.message = "", T.progWidth = n }

unset_width :: T.SettingAction
unset_width _ = pure D.noMessage

---------------------------------------------------------------------
