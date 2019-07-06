{-# LANGUAGE OverloadedStrings #-}

module Controller.KeyBindings
    ( keyBindings
    , parseKey
    ) where

import qualified Graphics.Vty               as V
import qualified Model.Types                as T
import qualified Model.Debugger.Debugger    as D
import Data.List                                    ( find )

parseKey :: V.Key -> T.Mode -> T.WgtName -> T.DebuggerCommand
parseKey k m w = maybe missing go . find ( (== k) . T.keyBind ) $ keyBindings
    where missing = T.PureCmd id
          go b    = T.keyAction b m w

keyBindings :: [T.KeyBinding]
keyBindings = [ T.KeyBinding V.KEsc         esc_action    esc_help
              -- T.KeyBinding V.KRight       right_action  right_help
              -- , T.KeyBinding V.KLeft        left_action   left_help
              -- , T.KeyBinding V.KUp          up_action     up_help
              -- , T.KeyBinding V.KDown        down_action   down_help
              -- , T.KeyBinding V.KPageDown    pgDown_action pgDown_help
              -- , T.KeyBinding V.KPageUp      pgUp_action   pgUp_action
              -- , T.KeyBinding (V.KChar ' ')  space_action  space_help
              -- , T.KeyBinding (V.KBS      )  bs_action     bs_help
              -- , T.KeyBinding (V.KChar '\t') tap_action    tab_help
                -- lower case characters
              , T.KeyBinding (V.KChar 'h')  h_action      h_help
              , T.KeyBinding (V.KChar 'k')  k_action      k_help
              , T.KeyBinding (V.KChar 'l')  l_action      l_help
              , T.KeyBinding (V.KChar 'j')  j_action      j_help
              , T.KeyBinding (V.KChar 't')  t_action      t_help
              -- , T.KeyBinding (V.KChar 'x')  x_action      x_help
                -- shift-characters
              -- , T.KeyBinding (V.KChar 'H')  shiftH_action shiftH_help
              -- , T.KeyBinding (V.KChar 'T')  shiftT_action shiftT_help
              -- , T.KeyBinding (V.KChar 'L')  shiftL_action shiftL_help
              -- , T.KeyBinding (V.KChar 'J')  shiftJ_action shiftJ_help
              --   -- non-alpha-numeric characters
              -- , T.KeyBinding (V.KChar '+')  plus_action   plus_help
              -- , T.KeyBinding (V.KChar '.')  dot_action    dot_help
              -- , T.KeyBinding (V.KChar ',')  comma_action  comma_help
              -- , T.KeyBinding (V.KChar ':')  colon_action  colon_help
              -- , T.KeyBinding (V.KChar '<')  lagl_action   lagl_help
              -- , T.KeyBinding (V.KChar '>')  gagl_action   gagl_help
              -- , T.KeyBinding (V.KChar '[')  lbra_action   lbra_help
              -- , T.KeyBinding (V.KChar ']')  rbra_action   rbra_help
              ]

---------------------------------------------------------------------

esc_help :: T.HelpInfo
esc_help = "help for <esc>"

esc_action :: T.Mode -> T.WgtName -> T.DebuggerCommand
esc_action _ _ = T.QuitCmd

---------------------------------------------------------------------

h_help :: T.HelpInfo
h_help = "help for h"

h_action :: T.Mode -> T.WgtName -> T.DebuggerCommand
h_action T.NormalMode T.ProgramWgt = T.PureCmd D.moveCursorLeft
h_action _            _            = T.PureCmd id

---------------------------------------------------------------------

j_help :: T.HelpInfo
j_help = "help for j"

j_action :: T.Mode -> T.WgtName -> T.DebuggerCommand
j_action T.NormalMode T.ProgramWgt = T.PureCmd D.moveCursorDown
j_action _            _            = T.PureCmd id

---------------------------------------------------------------------

k_help :: T.HelpInfo
k_help = "help for k"

k_action :: T.Mode -> T.WgtName -> T.DebuggerCommand
k_action T.NormalMode T.ProgramWgt = T.PureCmd D.moveCursorUp
k_action _            _            = T.PureCmd id

---------------------------------------------------------------------

l_help :: T.HelpInfo
l_help = "help for l"

l_action :: T.Mode -> T.WgtName -> T.DebuggerCommand
l_action T.NormalMode T.ProgramWgt = T.PureCmd D.moveCursorRight
l_action _            _            = T.PureCmd id

---------------------------------------------------------------------

t_help :: T.HelpInfo
t_help = "help for t"

t_action T.NormalMode T.ProgramWgt = T.PureCmd D.moveCursorRight
t_action _            _            = T.PureCmd id

---------------------------------------------------------------------


---------------------------------------------------------------------

--pKeyEvent V.KLeft        _ db = B.continue . D.moveCursorLeft  $ db
--pKeyEvent V.KUp          _ db = B.continue . D.moveCursorUp    $ db
--pKeyEvent V.KDown        _ db = B.continue . D.moveCursorDown  $ db
--pKeyEvent (V.KChar 'h')  _ db = B.continue . D.moveCursorLeft  $ db
--pKeyEvent (V.KChar 'l')  _ db = B.continue . D.moveCursorRight $ db
--pKeyEvent (V.KChar 'k')  _ db = B.continue . D.moveCursorUp    $ db
--pKeyEvent (V.KChar 'j')  _ db = B.continue . D.moveCursorDown  $ db
--pKeyEvent (V.KChar 't')  _ db = B.continue . D.moveCursorRight $ db
--  -- Program single steps: these should be fast and will not hang
--pKeyEvent (V.KChar ' ')  _ db = B.continue . D.stepForward  $ db
--pKeyEvent V.KBS          _ db = B.continue . D.stepBackward $ db
--pKeyEvent (V.KChar 'H')  _ db = B.continue . D.stepBackward $ db
--pKeyEvent (V.KChar 'T')  _ db = B.continue . D.stepForward  $ db
--pKeyEvent (V.KChar 'L')  _ db = B.continue . D.stepForward  $ db
--  -- Progam jumps: these may be slow or non-halting
--pKeyEvent (V.KPageDown)  _ db = isolate D.jumpForward  db
--pKeyEvent (V.KPageUp  )  _ db = isolate D.jumpBackward db
--pKeyEvent (V.KChar 'J')  _ db = isolate D.jumpForward  db
--pKeyEvent (V.KChar 'K')  _ db = isolate D.jumpBackward db
--  -- Program editing
--pKeyEvent (V.KChar 'x')  _ db = B.continue . D.deleteStatementAtCursor       $ db
--pKeyEvent (V.KChar '<')  _ db = B.continue . D.addAtCursor T.DBBackup        $ db
--pKeyEvent (V.KChar '>')  _ db = B.continue . D.addAtCursor T.DBAdvance       $ db
--pKeyEvent (V.KChar '+')  _ db = B.continue . D.addAtCursor T.DBIncrement     $ db
--pKeyEvent (V.KChar '-')  _ db = B.continue . D.addAtCursor T.DBDecrement     $ db
--pKeyEvent (V.KChar '.')  _ db = B.continue . D.addAtCursor T.DBWriteOut      $ db
--pKeyEvent (V.KChar ',')  _ db = B.continue . D.addAtCursor T.DBReadIn        $ db
--pKeyEvent (V.KChar '[')  _ db = B.continue . D.addAtCursor (T.DBOpenLoop 0)  $ db
--pKeyEvent (V.KChar ']')  _ db = B.continue . D.addAtCursor (T.DBCloseLoop 0) $ db
--  -- Entering command mode
--pKeyEvent (V.KChar ':' ) _ db = B.continue $ db { T.mode = T.CommandMode }
--  -- Tabbing between widgets
--pKeyEvent (V.KChar '\t') _ db = let newFocus = D.nextWidget . T.wgtFocus $ db
--                                in  B.continue $ db { T.wgtFocus = newFocus }
--pKeyEvent _              _ db = B.continue db
--
--keyEvent :: V.Key -> T.Debugger -> DebugEventMonad
--keyEvent k db = case parseKey k db of
--                     T.PureCmd f      -> B.continue . f $ db
--                     T.QuitCmd        -> B.halt db
--                     T.ErrorCmd e     -> B.continue $ db { T.message = e }
--                     T.SimpleIOCmd f  -> B.continue db
--                     T.ComplexIOCmd f -> B.continue db
--                     T.TandemIOCmd f  -> B.continue db
