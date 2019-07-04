{-# LANGUAGE OverloadedStrings #-}

module View.Help
    ( helpWidget
    ) where

import qualified Data.Text   as Tx
import qualified Brick       as B
import qualified Model.Types as T
import Data.Text                    ( Text         )
import Data.List                    ( intersperse  )
import Brick                        ( (<=>), (<+>) )
import Controller.Commands          ( commands     )

-- =============================================================== --
-- Rendering the help UI for displaying help and other information

helpWidget :: [String] -> B.Widget T.WgtName
helpWidget [] =
    let hdr = B.txt "Welcome to the Defugger! A BF Debugger!"
        bdy = B.txt mainHelpTxt
    in  B.viewport T.HelpWgt B.Both $
        B.withAttr "header" hdr <=> spacer <=> bdy

helpWidget ("keys":_) =
    B.viewport T.HelpWgt B.Both
        $ B.txt "Help for key-bindings is still being implement."
          <=> spacer
          <=> B.txt "Esc to return."

helpWidget ("settings":_) =
    B.viewport T.HelpWgt B.Both
        $ B.txt "Help for settings is still being implemented."
          <=> spacer
          <=> B.txt "Esc to return."

helpWidget ("commands":_) =
    B.viewport T.HelpWgt B.Both
        $ B.txt "Help is still being implemented, so this is incomplete."
          <=> spacer
          <=> B.txt "Esc to return."
          <=> spacer
          <=> (B.vBox . map shortHelp) commands

helpWidget _ =
    B.viewport T.HelpWgt B.Both
        $ B.txt "Sorry! This is still being implement."

---------------------------------------------------------------------

shortHelp :: T.Command -> B.Widget T.WgtName
shortHelp (T.Command ns _ sh _) = names <+> B.txt " " <+> summary
    where summary = B.txt sh
          names   = B.hBox . intersperse (B.str "/")
                    . map (B.withAttr "command" . B.str) $ ns

spacer :: B.Widget T.WgtName
spacer = B.txt " "

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

