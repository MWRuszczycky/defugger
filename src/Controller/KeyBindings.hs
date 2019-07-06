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
              , T.KeyBinding V.KRight       right_action  right_help
              , T.KeyBinding V.KLeft        left_action   left_help
              , T.KeyBinding V.KUp          up_action     up_help
              , T.KeyBinding V.KDown        down_action   down_help
              , T.KeyBinding V.KPageDown    pgDown_action pgDown_help
              , T.KeyBinding V.KPageUp      pgUp_action   pgUp_help
              , T.KeyBinding (V.KChar ' ')  space_action  space_help
              , T.KeyBinding (V.KBS      )  bs_action     bs_help
              , T.KeyBinding (V.KChar '\t') tab_action    tab_help
                -- lower case characters
              , T.KeyBinding (V.KChar 'h')  h_action      h_help
              , T.KeyBinding (V.KChar 'k')  k_action      k_help
              , T.KeyBinding (V.KChar 'l')  l_action      l_help
              , T.KeyBinding (V.KChar 'j')  j_action      j_help
              , T.KeyBinding (V.KChar 'q')  q_action      q_help
              , T.KeyBinding (V.KChar 't')  t_action      t_help
              , T.KeyBinding (V.KChar 'x')  x_action      x_help
                -- shift-characters
              , T.KeyBinding (V.KChar 'H')  shiftH_action shiftH_help
              , T.KeyBinding (V.KChar 'J')  shiftJ_action shiftJ_help
              , T.KeyBinding (V.KChar 'K')  shiftK_action shiftK_help
              , T.KeyBinding (V.KChar 'L')  shiftL_action shiftL_help
              , T.KeyBinding (V.KChar 'T')  shiftT_action shiftT_help
              --   -- non-alpha-numeric characters
              , T.KeyBinding (V.KChar '+')  plus_action   plus_help
              , T.KeyBinding (V.KChar '-')  minus_action  minus_help
              , T.KeyBinding (V.KChar '.')  dot_action    dot_help
              , T.KeyBinding (V.KChar ',')  comma_action  comma_help
              , T.KeyBinding (V.KChar ':')  colon_action  colon_help
              , T.KeyBinding (V.KChar '<')  langle_action langle_help
              , T.KeyBinding (V.KChar '>')  rangle_action rangle_help
              , T.KeyBinding (V.KChar '[')  lbra_action   lbra_help
              , T.KeyBinding (V.KChar ']')  rbra_action   rbra_help
              ]

type KeyAction = T.Mode -> T.WgtName -> T.DebuggerCommand

-- right ------------------------------------------------------------

right_help :: T.HelpInfo
right_help = "help for <right>"

right_action :: KeyAction
right_action T.NormalMode   T.ProgramWgt = T.PureCmd D.moveCursorRight
right_action T.NormalMode   T.InputWgt   = T.HScrollCmd T.InputWgt  1
right_action T.NormalMode   T.OutputWgt  = T.HScrollCmd T.OutputWgt 1
right_action (T.HelpMode _) _            = T.HScrollCmd T.HelpWgt   1
right_action _              _            = T.PureCmd id

-- left -------------------------------------------------------------

left_help :: T.HelpInfo
left_help = "help for <left>"

left_action :: KeyAction
left_action T.NormalMode   T.ProgramWgt = T.PureCmd D.moveCursorLeft
left_action T.NormalMode   T.InputWgt   = T.HScrollCmd T.InputWgt  (-1)
left_action T.NormalMode   T.OutputWgt  = T.HScrollCmd T.OutputWgt (-1)
left_action (T.HelpMode _) _            = T.HScrollCmd T.HelpWgt   (-1)
left_action _              _            = T.PureCmd id

-- up ---------------------------------------------------------------

up_help :: T.HelpInfo
up_help = "help for <up>"

up_action :: KeyAction
up_action T.NormalMode   T.ProgramWgt = T.PureCmd D.moveCursorUp
up_action T.NormalMode   T.InputWgt   = T.VScrollCmd T.InputWgt  (-1)
up_action T.NormalMode   T.OutputWgt  = T.VScrollCmd T.OutputWgt (-1)
up_action (T.HelpMode _) _            = T.VScrollCmd T.HelpWgt   (-1)
up_action T.NormalMode   T.MemoryWgt  = T.PureCmd $ D.scrollMemView (-1)
up_action _              _            = T.PureCmd id

-- down -------------------------------------------------------------

down_help :: T.HelpInfo
down_help = "help for <down>"

down_action :: KeyAction
down_action T.NormalMode   T.ProgramWgt = T.PureCmd D.moveCursorDown
down_action T.NormalMode   T.InputWgt   = T.VScrollCmd T.InputWgt  1
down_action T.NormalMode   T.OutputWgt  = T.VScrollCmd T.OutputWgt 1
down_action (T.HelpMode _) _            = T.VScrollCmd T.HelpWgt   1
down_action T.NormalMode   T.MemoryWgt  = T.PureCmd $ D.scrollMemView 1
down_action _              _            = T.PureCmd id

-- page-up ----------------------------------------------------------

pgUp_help :: T.HelpInfo
pgUp_help = "help for <page-up>"

pgUp_action :: KeyAction
pgUp_action T.NormalMode T.ProgramWgt = T.TandemCmd D.jumpBackward
pgUp_action _            _            = T.PureCmd id

-- page-down --------------------------------------------------------

pgDown_help :: T.HelpInfo
pgDown_help = "help for <page-down>"

pgDown_action :: KeyAction
pgDown_action T.NormalMode T.ProgramWgt = T.TandemCmd D.jumpForward
pgDown_action _            _            = T.PureCmd id

-- space ------------------------------------------------------------

space_help :: T.HelpInfo
space_help = "help for <space>"

space_action :: KeyAction
space_action T.NormalMode T.ProgramWgt = T.PureCmd D.stepForward
space_action _            _            = T.PureCmd id

-- bs ---------------------------------------------------------------

bs_help :: T.HelpInfo
bs_help = "help for <back-space>"

bs_action :: KeyAction
bs_action T.NormalMode T.ProgramWgt = T.PureCmd D.stepBackward
bs_action _            _            = T.PureCmd id

-- tab --------------------------------------------------------------

tab_help :: T.HelpInfo
tab_help = "help for <tab>"

tab_action :: KeyAction
tab_action T.NormalMode w = T.PureCmd $ \ db -> db {T.wgtFocus = D.nextWidget w}
tab_action _            _ = T.PureCmd id

-- esc --------------------------------------------------------------

esc_help :: T.HelpInfo
esc_help = "help for <esc>"

esc_action :: T.Mode -> T.WgtName -> T.DebuggerCommand
esc_action (T.HelpMode _) _ = T.PureCmd $ \ db -> db { T.mode = T.NormalMode }
esc_action _              _ = T.QuitCmd

-- =============================================================== --
-- Shifted characters

-- H ----------------------------------------------------------------

shiftH_help :: T.HelpInfo
shiftH_help = "help for H"

shiftH_action :: KeyAction
shiftH_action T.NormalMode T.ProgramWgt = T.PureCmd D.stepBackward
shiftH_action _            _            = T.PureCmd id

-- J ----------------------------------------------------------------

shiftJ_help :: T.HelpInfo
shiftJ_help = "help for J"

shiftJ_action :: KeyAction
shiftJ_action T.NormalMode T.ProgramWgt = T.TandemCmd D.jumpForward
shiftJ_action _            _            = T.PureCmd id

-- K ----------------------------------------------------------------

shiftK_help :: T.HelpInfo
shiftK_help = "help for K"

shiftK_action :: KeyAction
shiftK_action T.NormalMode T.ProgramWgt = T.TandemCmd D.jumpBackward
shiftK_action _            _            = T.PureCmd id

-- L ----------------------------------------------------------------

shiftL_help :: T.HelpInfo
shiftL_help = "help for L"

shiftL_action :: KeyAction
shiftL_action T.NormalMode T.ProgramWgt = T.PureCmd D.stepForward
shiftL_action _            _            = T.PureCmd id

-- T ----------------------------------------------------------------

shiftT_help :: T.HelpInfo
shiftT_help = "help for T"

shiftT_action :: KeyAction
shiftT_action T.NormalMode T.ProgramWgt = T.PureCmd D.stepForward
shiftT_action _            _            = T.PureCmd id

-- =============================================================== --
-- Lower case characters

-- h ----------------------------------------------------------------

h_help :: T.HelpInfo
h_help = "help for h"

h_action :: T.Mode -> T.WgtName -> T.DebuggerCommand
h_action T.NormalMode   T.ProgramWgt = T.PureCmd D.moveCursorLeft
h_action T.NormalMode   T.InputWgt   = T.HScrollCmd T.InputWgt  (-1)
h_action T.NormalMode   T.OutputWgt  = T.HScrollCmd T.OutputWgt (-1)
h_action (T.HelpMode _) _            = T.HScrollCmd T.HelpWgt   (-1)
h_action _              _            = T.PureCmd id

-- j ----------------------------------------------------------------

j_help :: T.HelpInfo
j_help = "help for j"

j_action :: T.Mode -> T.WgtName -> T.DebuggerCommand
j_action T.NormalMode   T.ProgramWgt = T.PureCmd D.moveCursorDown
j_action T.NormalMode   T.InputWgt   = T.VScrollCmd T.InputWgt  1
j_action T.NormalMode   T.OutputWgt  = T.VScrollCmd T.OutputWgt 1
j_action (T.HelpMode _) _            = T.VScrollCmd T.HelpWgt   1
j_action T.NormalMode   T.MemoryWgt  = T.PureCmd $ D.scrollMemView 1
j_action _              _            = T.PureCmd id

-- k ----------------------------------------------------------------

k_help :: T.HelpInfo
k_help = "help for k"

k_action :: T.Mode -> T.WgtName -> T.DebuggerCommand
k_action T.NormalMode   T.ProgramWgt = T.PureCmd D.moveCursorUp
k_action T.NormalMode   T.InputWgt   = T.VScrollCmd T.InputWgt  (-1)
k_action T.NormalMode   T.OutputWgt  = T.VScrollCmd T.OutputWgt (-1)
k_action (T.HelpMode _) _            = T.VScrollCmd T.HelpWgt   (-1)
k_action T.NormalMode   T.MemoryWgt  = T.PureCmd $ D.scrollMemView (-1)
k_action _              _            = T.PureCmd id

-- l ----------------------------------------------------------------

l_help :: T.HelpInfo
l_help = "help for l"

l_action :: T.Mode -> T.WgtName -> T.DebuggerCommand
l_action T.NormalMode   T.ProgramWgt = T.PureCmd D.moveCursorRight
l_action T.NormalMode   T.InputWgt   = T.HScrollCmd T.InputWgt  1
l_action T.NormalMode   T.OutputWgt  = T.HScrollCmd T.OutputWgt 1
l_action (T.HelpMode _) _            = T.HScrollCmd T.HelpWgt   1
l_action _              _            = T.PureCmd id

-- q ----------------------------------------------------------------

q_help :: T.HelpInfo
q_help = "help for q"

q_action :: T.Mode -> T.WgtName -> T.DebuggerCommand
q_action (T.HelpMode _) _ = T.PureCmd $ \ db -> db { T.mode = T.NormalMode }
q_action _              _ = T.PureCmd id

-- t ----------------------------------------------------------------

t_help :: T.HelpInfo
t_help = "help for t"

t_action :: T.Mode -> T.WgtName -> T.DebuggerCommand
t_action T.NormalMode   T.ProgramWgt = T.PureCmd D.moveCursorRight
t_action T.NormalMode   T.InputWgt   = T.HScrollCmd T.InputWgt  1
t_action T.NormalMode   T.OutputWgt  = T.HScrollCmd T.OutputWgt 1
t_action (T.HelpMode _) _            = T.HScrollCmd T.HelpWgt   1
t_action _              _            = T.PureCmd id

-- x ----------------------------------------------------------------

x_help :: T.HelpInfo
x_help = "help for x"

x_action :: KeyAction
x_action T.NormalMode T.ProgramWgt = T.PureCmd D.deleteStatementAtCursor
x_action _            _            = T.PureCmd id

-- ================================================================ --
-- Non-alpha-numeric characters

-- + / <plus> -------------------------------------------------------

plus_help :: T.HelpInfo
plus_help = "help for +"

plus_action :: KeyAction
plus_action T.NormalMode T.ProgramWgt = T.PureCmd $ D.addAtCursor T.DBIncrement
plus_action _            _            = T.PureCmd id

-- - / <minus> ------------------------------------------------------

minus_help :: T.HelpInfo
minus_help = "help for -"

minus_action :: KeyAction
minus_action T.NormalMode T.ProgramWgt = T.PureCmd $ D.addAtCursor T.DBDecrement
minus_action _            _            = T.PureCmd id

-- . / <dot> --------------------------------------------------------

dot_help :: T.HelpInfo
dot_help = "help for ."

dot_action :: KeyAction
dot_action T.NormalMode T.ProgramWgt = T.PureCmd $ D.addAtCursor T.DBWriteOut
dot_action _            _            = T.PureCmd id

-- , / <comma> ------------------------------------------------------

comma_help :: T.HelpInfo
comma_help = "help for ,"

comma_action :: KeyAction
comma_action T.NormalMode T.ProgramWgt = T.PureCmd $ D.addAtCursor T.DBReadIn
comma_action _            _            = T.PureCmd id

-- : / <colon> ------------------------------------------------------

colon_help :: T.HelpInfo
colon_help = "help for :"

colon_action :: KeyAction
colon_action T.NormalMode _ = T.PureCmd $ \ db -> db { T.mode = T.CommandMode }
colon_action _            _ = T.PureCmd id

-- < / <langle> -----------------------------------------------------

langle_help :: T.HelpInfo
langle_help = "help for <"

langle_action :: KeyAction
langle_action T.NormalMode T.ProgramWgt = T.PureCmd $ D.addAtCursor T.DBBackup
langle_action _            _            = T.PureCmd id

-- > / <rangle> -----------------------------------------------------

rangle_help :: T.HelpInfo
rangle_help = "help for >"

rangle_action :: KeyAction
rangle_action T.NormalMode T.ProgramWgt = T.PureCmd $ D.addAtCursor T.DBAdvance
rangle_action _            _            = T.PureCmd id

-- [ / <lbra> -------------------------------------------------------

lbra_help :: T.HelpInfo
lbra_help = "help for ["

lbra_action T.NormalMode T.ProgramWgt = T.PureCmd $ D.addAtCursor
                                                  $ T.DBOpenLoop 0
lbra_action _            _            = T.PureCmd id

-- ] / <rbra> -------------------------------------------------------

rbra_help :: T.HelpInfo
rbra_help = "help for ]"

rbra_action T.NormalMode T.ProgramWgt = T.PureCmd $ D.addAtCursor
                                                  $ T.DBCloseLoop 0
rbra_action _            _            = T.PureCmd id

---------------------------------------------------------------------
