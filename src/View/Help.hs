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
import Data.Text                    ( Text                   )
import Data.List                    ( intersperse, intersect )
import Brick                        ( (<=>), (<+>)           )
import Controller.Commands          ( commands               )
import Controller.Settings          ( settings               )

-- =============================================================== --
-- The main help-UI widget

helpWidget :: [String] -> B.Widget T.WgtName
helpWidget [] =
    let header = B.txt "Welcome to the Defugger! A BF Debugger!"
        body   = B.txt mainHelpTxt
    in  B.viewport T.HelpWgt B.Both $
        B.withAttr "header" header <=> spacer 1 <=> body

helpWidget ("keys":_) =
    let header = B.txt "List of key-bindings currently available."
        body   = B.vBox . map keyBindingSummaryWidget $ keyBindings
    in  B.viewport T.HelpWgt B.Both
            $ B.withAttr "header" header
              <=> spacer 1
              <=> ( spacer 2 <+> body )

helpWidget ("settings":_) =
    let header = B.txt "List of settings currently available"
        note   = B.txt "Settings used as arguments to the set & unset commands"
        summaries = B.vBox . map settingSummaryWidget $ settings
    in  B.viewport T.HelpWgt B.Both
            $ B.withAttr "header" header
              <=> note
              <=> spacer 1
              <=> ( spacer 2 <+> summaries )

helpWidget ("commands":_) =
    let header    = B.txt "List of commands currently available"
        note      = B.txt "For details about a command, :help command-name"
        summaries = B.vBox . map commandSummaryWidget $ commands
    in B.viewport T.HelpWgt B.Both
           $ B.withAttr "header" header
             <=> note
             <=> spacer 1
             <=> ( spacer 2 <+> summaries )

helpWidget cs = let ws      = filter matches commands
                    matches = not . null . intersect cs . T.cmdNames
                in  B.viewport T.HelpWgt B.Both
                    $ B.vBox . intersperse (spacer 1)
                      . map commandDetailsWidget $ ws

-- =============================================================== --
-- Widget constructors

spacer :: Int -> B.Widget T.WgtName
spacer n = B.txt . Tx.replicate n $ " "

commandSummaryWidget :: T.Command -> B.Widget T.WgtName
commandSummaryWidget (T.Command ns _ sh _) = names <=> ( spacer 2 <+> summary )
    where summary = B.txt sh
          names   = B.hBox . intersperse (B.txt " | ")
                    . map (B.withAttr "command" . B.str) $ ns

settingSummaryWidget :: T.Setting -> B.Widget T.WgtName
settingSummaryWidget (T.Setting n _ _ h) = name <=> ( spacer 2 <+> summary )
    where summary = B.txt h
          name    = B.withAttr "setting" . B.str $ n

commandDetailsWidget :: T.Command -> B.Widget T.WgtName
commandDetailsWidget c = header <=> spacer 1 <=> ( spacer 2 <+> details )
    where header  = commandSummaryWidget c
          details = B.txt . T.longHelp $ c

keyBindingSummaryWidget :: ([Text], Text) -> B.Widget T.WgtName
keyBindingSummaryWidget (ks, x) = header <=> ( spacer 2 <+> action )
    where action = B.txt x
          header = B.hBox . intersperse (B.txt " | ")
                   . map (B.withAttr "keybinding" . B.txt) $ ks

-- =============================================================== --
-- Default key binding help strings

keyBindings :: [([Text], Text)]
keyBindings = [ ( [ "<esc>" ],
                    "Normal Mode: quit the Defugger or abort jump.\n"
                    <> "Command Mode: abort command.\n"
                    <> "Help Mode: return to Normal Mode." )
              , ( [ "q" ],
                    "Return to Normal Mode from Help Mode." )
              , ( [ "<right-arrow>", "l", "t" ],
                    "Move cursor one statement forward in Program Window.\n"
                    <> "Scroll Output and Input Windows.\n"
                    <> "Scroll through help." )
              , ( [ "<left-arrow>", "h" ],
                    "Move cursor one statement backward in Program Window.\n"
                    <> "Scroll Output and Input Windows.\n"
                    <> "Scroll through help." )
              , ( [ "<down-arrow>", "j" ],
                    "Move cursor to the next row in Program Window.\n"
                    <> "Scroll Memory, Output and Input Windows.\n"
                    <> "Scroll through help." )
              , ( [ "<up-arrow>", "k" ],
                    "Move cursor to previous row in Program Window.\n"
                    <> "Scroll Memory, Output and Input Windows.\n"
                    <> "Scroll through help." )
              , ( [ "<space>", "L", "T" ],
                    "Advance program execution one statement forward." )
              , ( [ "<back-space>", "H" ],
                    "Revert program execution one statement backward." )
              , ( [ "<page-down>", "J" ],
                    "Jump forward to next break point." )
              , ( [ "<page-up>", "K" ],
                    "Jump backward to previous break point." )
              , ( [ "x" ],
                    "Delete statement at cursor." )
              , ( [ ">" ],
                    "Insert 'advance'/'>' BF statement at cursor." )
              , ( [ "<" ],
                    "Insert 'backup'/'<' BF statement at cursor." )
              , ( [ "+" ],
                    "Insert 'increment'/'+' BF statement at cursor." )
              , ( [ "-" ],
                    "Insert 'decrement'/'-' BF statement at cursor." )
              , ( [ "." ],
                    "Insert 'write'/'.' BF statement at cursor." )
              , ( [ "," ],
                    "Insert 'read'/',' BF statement at cursor." )
              , ( [ "[", "]" ],
                    "Insert 'loop'/'[]' BF condition loop at cursor." )
              , ( [ ":" ],
                    "Enter Command Mode to run a typed command." )
              , ( [ "<tab>" ],
                    "Cycle between windows in Normal Mode." )
              ]

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

