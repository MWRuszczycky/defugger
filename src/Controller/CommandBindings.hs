{-# LANGUAGE OverloadedStrings #-}

module Controller.CommandBindings
    ( parseCommand
    , commands
    ) where

-- =============================================================== --
-- User commands that can be entered and run in debugger mode      --
-- =============================================================== --

import qualified Model.Types             as T
import qualified Data.Text               as Tx
import Data.Text                                ( Text           )
import Control.Applicative                      ( (<|>)          )
import Data.Maybe                               ( listToMaybe    )
import Data.Foldable                            ( toList         )
import Control.Monad.Except                     ( throwError     )
import Data.List                                ( find           )
import Model.Utilities                          ( chunksOf       )
import Model.CoreIO                             ( tryWriteFile   )
import Controller.Settings                      ( parseSet
                                                , parseUnset     )
import Controller.Loader                        ( reloadDebugger
                                                , resetDebugger  )

-- =============================================================== --
-- Command hub and router

parseCommand :: [Text] -> T.DebuggerCommand
parseCommand []     = T.PureCmd $ id
parseCommand (x:xs) = maybe err go . find ( elem x . T.cmdNames ) $ commands
    where err  = T.ErrorCmd "Command unrecognized"
          go c = T.cmdAction c xs

commands :: [T.CommandBinding]
-- ^Organizes all the commands that can be run from the debugger.
commands = [ T.CommandBinding ["help",  "h"] help_action  help_help
           , T.CommandBinding ["load",  "l"] load_action  load_help
           , T.CommandBinding ["reset", "r"] reset_action reset_help
           , T.CommandBinding ["set",   "s"] set_action   set_help
           , T.CommandBinding ["unset", "u"] unset_action unset_help
           , T.CommandBinding ["write", "w"] write_action write_help
           , T.CommandBinding ["quit",  "q"] quit_action  quit_help
           ]

-- =============================================================== --
-- Commands

-- help -------------------------------------------------------------

help_help :: T.HelpInfo
help_help = T.HelpInfo ns us sh lh
    where ns = [ "help", "h" ]
          us = ":help [COMMAND | KEY | SETTING]"
          sh = "Display help information"
          lh = Tx.empty

help_action :: T.CommandAction
help_action [] = T.PureCmd $ \ db -> db {T.mode = T.HelpMode Tx.empty}
help_action xs = T.PureCmd $ \ db -> db {T.mode = T.HelpMode . Tx.unwords $ xs}

-- load -------------------------------------------------------------

load_help :: T.HelpInfo
load_help = T.HelpInfo ns us sh lh
    where ns = [ "load", "l" ]
          us = ":load FILEPATH-BF [FILEPATH-INPUT]"
          sh = "Load BF script and input file into the Defugger"
          lh = Tx.empty

load_action :: T.CommandAction
load_action []      = T.ErrorCmd "A path to a BF script must be specified"
load_action (x:y:_) = T.SimpleIOCmd $ reloadDebugger ( Just . Tx.unpack $ x )
                                                     ( Just . Tx.unpack $ y )
load_action (x:_)   = T.SimpleIOCmd $ reloadDebugger ( Just . Tx.unpack $ x )
                                                     Nothing

-- reset ------------------------------------------------------------

reset_help :: T.HelpInfo
reset_help = T.HelpInfo ns us sh lh
    where ns = [ "reset", "r" ]
          us = ":reset"
          sh = "Reset the Defugger to its original state."
          lh = Tx.empty

reset_action :: T.CommandAction
reset_action _ = T.PureCmd $ resetDebugger

-- set --------------------------------------------------------------

set_help :: T.HelpInfo
set_help = T.HelpInfo ns us sh lh
    where ns = [ "set", "s" ]
          us = ":set [SETTING [VALUE..]]"
          sh = "Sets a property in the Defugger"
          lh = Tx.empty

set_action :: T.CommandAction
set_action = either T.ErrorCmd T.PureCmd . parseSet

-- unset ------------------------------------------------------------

unset_help :: T.HelpInfo
unset_help = T.HelpInfo ns us sh lh
    where ns = [ "unset", "u" ]
          us = ":unset [SETTING [VALUE..]]"
          sh = "Unsets a property in the Defugger"
          lh = Tx.empty

unset_action :: T.CommandAction
unset_action = either T.ErrorCmd T.PureCmd . parseUnset

-- write ------------------------------------------------------------

write_help :: T.HelpInfo
write_help = T.HelpInfo ns us sh lh
    where ns = [ "write", "w" ]
          us = ":write [FILEPATH]"
          sh = "Write the current BF script to memory"
          lh = Tx.empty

write_action :: T.CommandAction
write_action xs = T.SimpleIOCmd $ \ db -> go db $
                      Tx.unpack <$> listToMaybe xs <|> T.scriptPath db
    where fmt n = unlines . chunksOf n . init . tail . concatMap show . toList
          go _  Nothing   = throwError "Save path required"
          go db (Just fp) = do let s = fmt ( T.progWidth db ) . T.program $ db
                               tryWriteFile fp (Tx.pack s)
                               pure $ db { T.message    = "saved to " ++ fp
                                         , T.scriptPath = Just fp
                                         }

-- quit -------------------------------------------------------------

quit_help :: T.HelpInfo
quit_help = T.HelpInfo ns us sh lh
    where ns = [ "quit", "q" ]
          us = ":quit"
          sh = "Quits the Defugger"
          lh = Tx.empty

quit_action :: T.CommandAction
quit_action _ = T.QuitCmd
