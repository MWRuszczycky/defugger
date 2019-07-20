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
import qualified Model.Debugger.Debugger as D
import Data.Text                                ( Text           )
import Control.Applicative                      ( (<|>)          )
import Data.Maybe                               ( listToMaybe    )
import Control.Monad.Except                     ( throwError
                                                , liftEither     )
import Data.List                                ( find           )
import Model.CoreIO                             ( tryWriteFile
                                                , tryReadBytes
                                                , tryWriteBytes  )
import Model.Utilities                          ( toDebugPath    )
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
           , T.CommandBinding ["open",  "o"] open_action  open_help
           , T.CommandBinding ["reset", "r"] reset_action reset_help
           , T.CommandBinding ["save",  "s"] save_action  save_help
           , T.CommandBinding ["set"       ] set_action   set_help
           , T.CommandBinding ["unset"     ] unset_action unset_help
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

-- open -------------------------------------------------------------

open_help :: T.HelpInfo
open_help = T.HelpInfo ns us sh (Tx.unlines lh)
    where ns = [ "open", "o" ]
          us = ":open FILEPATH"
          sh = "Opens a debugger state file."
          lh = [ "Use the :open command to load script, memory, output and"
               , "input state information into the debugger. This information"
               , "can be generated using the :save command creating .defug"
               , "files. See also the :save, :write and :load commands."
               ]

open_action :: T.CommandAction
open_action []    = T.ErrorCmd "A path to a .defug file must be specified."
open_action (x:_) = T.SimpleIOCmd $ \ db ->
                        tryReadBytes (Tx.unpack x)
                        >>= liftEither . D.decodeDebugger db
                        >>= pure . D.noMessage . D.updateViewByPosition

-- reset ------------------------------------------------------------

reset_help :: T.HelpInfo
reset_help = T.HelpInfo ns us sh lh
    where ns = [ "reset", "r" ]
          us = ":reset"
          sh = "Reset the Defugger to its original state."
          lh = Tx.empty

reset_action :: T.CommandAction
reset_action _ = T.PureCmd $ resetDebugger

-- save -------------------------------------------------------------

save_help :: T.HelpInfo
save_help = T.HelpInfo ns us sh (Tx.unlines lh)
    where ns = [ "save", "s" ]
          us = ":save [FILEPATH]"
          sh = "Saves the computer state at the current point of evaluation."
          lh = [ "Use the :save command to save the script, memory, output and"
               , "input states at the current point of script evaluation to a"
               , "binary file. The file path to save to can be specified as the"
               , "FILEPATH argument. If no argument is provided, then the"
               , "current script path is used with the extension .defug. See"
               , "also help for the :open and :write commands."
               ]

save_action :: T.CommandAction
save_action xs = T.SimpleIOCmd $
    \ db -> maybe err (go db) $ Tx.unpack <$> listToMaybe xs
                                <|> (toDebugPath <$> T.scriptPath db)
    where err      = throwError "Save path required"
          go db fp = do tryWriteBytes fp . D.encodeDebugger $ db
                        pure $ db { T.message = "State saved to " ++ fp }

-- set --------------------------------------------------------------

set_help :: T.HelpInfo
set_help = T.HelpInfo ns us sh lh
    where ns = [ "set" ]
          us = ":set [SETTING [VALUE..]]"
          sh = "Sets a property in the Defugger"
          lh = Tx.empty

set_action :: T.CommandAction
set_action = either T.ErrorCmd T.PureCmd . parseSet

-- unset ------------------------------------------------------------

unset_help :: T.HelpInfo
unset_help = T.HelpInfo ns us sh lh
    where ns = [ "unset" ]
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
write_action xs = T.SimpleIOCmd $
    \ db -> maybe err (go db) $ Tx.unpack <$> listToMaybe xs <|> T.scriptPath db
    where err      = throwError "Save path required"
          go db fp = do tryWriteFile fp . D.programToText (T.progWidth db) $ db
                        pure $ db { T.message    = "saved to " <> fp
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
