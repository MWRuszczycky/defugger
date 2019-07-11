{-# LANGUAGE OverloadedStrings #-}

module View.Help
    ( helpWidget
    ) where

-- =============================================================== --
-- Rendering the help UI for displaying help and other information --
-- =============================================================== --

import qualified Data.Text   as Tx
import qualified Brick       as B
import qualified Model.Types as T
import Data.Text                    ( Text                          )
import Data.List                    ( intersperse, nubBy            )
import Brick                        ( (<=>), (<+>)                  )
import Controller.CommandBindings   ( commands                      )
import Controller.Settings          ( settings                      )
import Controller.KeyBindings       ( keyBindings                   )

-- =============================================================== --
-- The main help-UI widget

-- This still needs a lot work!

helpWidget :: Text -> B.Widget T.WgtName
helpWidget "" = mainHelpUI

helpWidget "keys" =
    summaryListHelpUI keyBindings
    "List of key-bindings currently available."
    Tx.empty

helpWidget "settings" =
    summaryListHelpUI settings
    "List of settings currently available"
    "Settings used as arguments to the set & unset commands"

helpWidget "commands" =
    summaryListHelpUI commands
    "List of commands currently available"
    "For details about a command, :help command-name"

helpWidget x =
    let isMatch = elem x . T.names . T.getHelp
    in  B.viewport T.HelpWgt B.Both $
            case nubBy sameHelp . filter isMatch $ everythingWithHelp of
                 [] -> missingHelpWgt x
                 hs -> B.vBox . intersperse (spacer 1 <=> rule)
                       . map detailsHelpWgt $ hs

-- =============================================================== --
-- Help UI widget constructors

---------------------------------------------------------------------
-- Main Help-UI widgets

mainHelpUI :: B.Widget T.WgtName
mainHelpUI =
    let header = B.txt "Welcome to the Defugger! A BF Debugger!"
        body   = B.txt mainHelpTxt
    in  B.viewport T.HelpWgt B.Both $
        B.withAttr "header" header <=> spacer 1 <=> body

summaryListHelpUI :: T.HasHelp a => [a] -> Text -> Text -> B.Widget T.WgtName
summaryListHelpUI xs header note =
    let body = B.vBox . map summaryHelpWgt . nubBy sameHelp $ xs
    in  B.viewport T.HelpWgt B.Both
            $ ( B.withAttr "header" . B.txt $ header )
              <=> B.txt note
              <=> spacer 1
              <=> ( spacer 2 <+> body )

---------------------------------------------------------------------
-- Component help widgets

summaryHelpWgt :: T.HasHelp a => a -> B.Widget T.WgtName
-- ^Display just the summary help information:
-- Names | What it is | How it is used | What it does
summaryHelpWgt x =
    let summary = B.txt . T.shortHelp . T.getHelp $ x
    in  ( B.hBox . intersperse (spacer 1) $
            [ namesWgt x
            , whatForWgt x
            , usageWgt x ] )
        <=> ( spacer 2 <+> summary )

detailsHelpWgt :: T.HasHelp a => a -> B.Widget T.WgtName
-- ^Display detailed help information:
-- Redisplay the summary help | Display the details.
detailsHelpWgt x = header <=> detWgt
    where header = summaryHelpWgt x
          detTxt = T.longHelp . T.getHelp $ x
          detWgt = if Tx.null detTxt
                      then B.emptyWidget
                      else spacer 1 <=> (spacer 2 <+> B.txt detTxt)

missingHelpWgt :: Text -> B.Widget T.WgtName
missingHelpWgt x = B.vBox $
    [ B.txt "There is no help available for "
      <+> (B.withAttr "error" . B.txt) x
    , B.txt "  For general help information, try just "
      <+> (B.withAttr "usage" . B.txt) ":help"
    , B.txt "  For a list of "
      <+> (B.withAttr "keybinding" . B.txt) "key-bindings"
      <+> B.txt " with help information, try "
      <+> (B.withAttr "usage" . B.txt) ":help keys"
    , B.txt "  For a list of "
      <+> (B.withAttr "command" . B.txt) "commands"
      <+> B.txt " with help information, try "
      <+> (B.withAttr "usage" . B.txt) ":help commands"
    , B.txt "  For a list of "
      <+> (B.withAttr "setting" . B.txt) "settings"
      <+> B.txt " with help information, try "
      <+> (B.withAttr "usage" . B.txt) ":help settings"
    ]

whatForWgt :: T.HasHelp a => a -> B.Widget T.WgtName
whatForWgt x = B.txt $ "(" <> T.helpFor x <> ")"

namesWgt :: T.HasHelp a => a -> B.Widget T.WgtName
namesWgt x = let helpInfo = T.getHelp x
                 style    = T.helpStyle x
             in  B.hBox . intersperse (B.txt " | ")
                 . map (B.withAttr style . B.txt)
                 . T.names $ helpInfo

usageWgt :: T.HasHelp a => a -> B.Widget T.WgtName
usageWgt x
    | Tx.null . T.usage $ h = B.emptyWidget
    | otherwise             = B.hBox ws
    where h  = T.getHelp x
          ws = [ B.txt "usage:"
               , spacer 1
               , B.withAttr "usage" . B.txt . T.usage $ h
               ]

-- =============================================================== --
-- Helpers

---------------------------------------------------------------------
-- Use an existential data type to generate a list of different data
-- types that all provide help information.

everythingWithHelp :: [T.HelpExists]
everythingWithHelp = concat [ map T.HelpExists commands
                            , map T.HelpExists settings
                            , map T.HelpExists keyBindings
                            ]

---------------------------------------------------------------------
-- Common operations for generating help UI widgets

spacer :: Int -> B.Widget T.WgtName
spacer n = B.txt . Tx.replicate n $ " "

rule :: B.Widget T.WgtName
rule = B.txt . Tx.replicate 80 $ "-"

sameHelp :: T.HasHelp a => a -> a -> Bool
sameHelp x y = let xHelp = T.getHelp x
                   yHelp = T.getHelp y
               in  T.names xHelp == T.names yHelp
                   && T.helpFor x == T.helpFor y

-- =============================================================== --
-- Large help strings

mainHelpTxt :: Text
mainHelpTxt = Tx.unlines hs
    where hs = [ "The Defugger is a Brain F**k (BF) debugger and interpretor"
               , "that runs entirely in your terminal.\n"
               , "Use the arrow keys to scroll through the Help text."
               , "To exit the help text, press the Esc-key."
               , "Press the Esc-key again to exit the program.\n"
               , "When running the debugger, you get the following windows:\n"
               , "  Program Window:"
               , "    This displays the BF program loaded in the debugger."
               , "    Code highlighted in red denotes break points that you can"
               , "    jump between. Code highlighted in yellow is the current"
               , "    location of the program execution. Code highlighted in"
               , "    green denotes the position of your cursor, which you can"
               , "    use to move around and set/unset break points, etc.\n"
               , "  Memory Window:"
               , "    This displays the current memory state of the computer"
               , "    given the BF code executed so far. The memory head is"
               , "    highlighted in yellow.\n"
               , "  Output Window:"
               , "    This dispalys any output the BF program has generated up"
               , "    to the current point of execution.\n"
               , "  Input Window:"
               , "    This displays any input the BF program has yet to"
               , "    consume.\n"
               , "At the bottom of the debugger is a status bar where errors"
               , "and other information is dispalyed. You can enter commands"
               , "here also by first pressing the colon (:) key. For example,"
               , "to set the Program Window to display only 50 BF characters"
               , "per line, you could run the following command:\n"
               , "  :set width 50\n"
               , "Use the <help> command to display help information:\n"
               , "  To display a list of all commands:"
               , "    :help commands\n"
               , "  To dispaly a list of all key-bindings:"
               , "    :help keys\n"
               , "  To display a list of all settings available:"
               , "    :help settings\n"
               , "To get detailed help about a specific command or setting:"
               , "  :help COMMAND | SETTING\n"
               , "where COMMAND or SETTING is the command or setting you are"
               , "are interested in learning more about.\n"
               , "Use the Esc-key to cancel a command, exit the help mode or"
               , "abort a long or non-halting computation.\n"
               , "To quit the Defugger debugger program, use the Esc-key from"
               , "Normal Mode or enter the command:\n"
               , "  :quit"
               ]

