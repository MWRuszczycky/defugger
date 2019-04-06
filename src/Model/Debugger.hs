module Model.Debugger
    ( -- Querying the debugger
      getAddress
    , getPosition
    , getPositionRow
    , getCursorRow
      -- Executing statements
    , stepForward
    , stepBackward
      -- Modeling widget interfaces
    , resize
    , nextWidget
      -- Cursor management
    , moveCursorRight
    , moveCursorLeft
    , moveCursorUp
    , moveCursorDown
    ) where

-- =============================================================== --
-- Combinators for manipulating and managing the Debugger model    --
-- =============================================================== --

import qualified Data.ByteString as BS
import qualified Model.Types     as T
import qualified Data.Vector     as Vec
import Data.Word                        ( Word8     )
import Data.Vector                      ( (!)       )
import Model.Interpreter                ( advance
                                        , backup
                                        , decrement
                                        , increment
                                        , readIn
                                        , writeOut  )

-- =============================================================== --
-- Querying the debugger

getAddress :: T.Debugger -> Int
-- ^Provide the index of the focus in the memory tape. This postioin
-- is highlighted in the memory UI.
getAddress db = let T.Tape xs _ _ = T.memory . T.computer $ db
                in  length xs

getPosition :: T.Debugger -> Int
-- ^Provide the index of the last statement executed in the program.
-- This corresponds to the current position of the debugger that is
-- highlighted in the program UI.
getPosition db = case T.history db of
                      []    -> 0
                      (x:_) -> x

getPositionRow :: T.Debugger -> Int
-- ^Provide the display row in the program widget where the program
-- is currently stopped (i.e., where the last statement executed is
-- displayed).
getPositionRow db = quot (getPosition db) (T.progWidth db)

getCursorRow :: T.Debugger -> Int
-- ^Provide the display row in the program widget where the cursor
-- can currently be found.
getCursorRow db = quot (T.cursor db) (T.progWidth db)

-- =============================================================== --
-- Executing statements

---------------------------------------------------------------------
-- Single steps

-- Exported

stepForward :: T.Debugger -> T.Debugger
-- ^Change the debugger state according to step forward command.
stepForward db = either (const db) id . executeNextStatement $ db

stepBackward :: T.Debugger -> T.Debugger
-- ^Change the debugger state according to step backward command.
stepBackward db = either (const db) id . revertLastStatement $ db

-- Unexported

executeNextStatement :: T.Debugger -> Either T.ErrString T.Debugger
-- ^Execute the next statement in the program if possible.
executeNextStatement db = updateHistory db
                          >>= updateBackup
                          >>= updateComputer
                          >>= pure . updateViewByPosition

revertLastStatement :: T.Debugger -> Either T.ErrString T.Debugger
-- ^Revert the last statement executed in the program if possible.
revertLastStatement db = revertComputer db
                         >>= revertBackup
                         >>= revertHistory
                         >>= pure . updateViewByPosition

---------------------------------------------------------------------
-- Managing debugger history
-- The history track what statements in the program have been so far
-- executed and in what order. This allows backtracking.

updateHistory :: T.Debugger -> Either T.ErrString T.Debugger
updateHistory db = pure $ db { T.history = h' }
    where n   = getPosition db
          h   = T.history db
          foc = T.focus . T.memory . T.computer $ db
          h'  = case T.program db ! n of
                     T.DBEnd         -> h
                     T.DBCloseLoop y -> y:h
                     T.DBOpenLoop  y -> if foc == 0 then (y+1):h else (n+1):h
                     _               -> (n+1):h

revertHistory :: T.Debugger -> Either T.ErrString T.Debugger
revertHistory db = case T.history db of
                        []    -> pure db
                        (_:h) -> pure $ db { T.history = h }

---------------------------------------------------------------------
-- Managing debugger backup memory
-- The backup memory is used to track what has been read from input
-- to the computer memory. This is necessary to ensure the orginal
-- memory state can be recoverd when reverting statements.

updateBackup :: T.Debugger -> Either T.ErrString T.Debugger
updateBackup db =
    let bs = T.readBackup db
        u  = T.focus . T.memory . T.computer $ db
    in  case T.program db ! (getPosition db) of
             T.DBReadIn  -> pure $ db { T.readBackup = u:bs }
             _           -> pure db

revertBackup :: T.Debugger -> Either T.ErrString T.Debugger
revertBackup db =
    case T.program db ! (getPosition db) of
         T.DBReadIn -> case T.readBackup db of
                            []     -> pure $ db { T.readBackup = [] }
                            (_:ws) -> pure $ db { T.readBackup = ws }
         _          -> pure db

---------------------------------------------------------------------
-- Managing the debugger computer model

updateComputer :: T.Debugger -> Either T.ErrString T.Debugger
updateComputer db = ( \ x -> db { T.computer = x } ) <$> c'
    where n  = getPosition db
          c  = T.computer db
          c' = case T.program db ! n of
                    T.DBIncrement -> increment c
                    T.DBDecrement -> decrement c
                    T.DBAdvance   -> advance   c
                    T.DBBackup    -> backup    c
                    T.DBReadIn    -> readIn    c
                    T.DBWriteOut  -> writeOut  c
                    _             -> pure      c

revertComputer :: T.Debugger -> Either T.ErrString T.Debugger
revertComputer db = ( \ x -> db { T.computer = x } ) <$> c'
    where c  = T.computer db
          n  = getPosition db
          c' = case T.program db ! n of
                    T.DBIncrement -> decrement c
                    T.DBDecrement -> increment c
                    T.DBAdvance   -> backup    c
                    T.DBBackup    -> advance   c
                    T.DBWriteOut  -> revertWriteOut c
                    T.DBReadIn    -> revertReadIn (T.readBackup db) $ c
                    _             -> pure      c

revertWriteOut :: T.Computer -> Either T.ErrString T.Computer
revertWriteOut c = maybe err go . BS.unsnoc . T.output $ c
    where err        = Left "At start of output, cannot revert write"
          go (out,_) = pure $ c { T.output = out }

revertReadIn :: [Word8] -> T.Computer -> Either T.ErrString T.Computer
revertReadIn []    _ = Left "At start of read backup, cannot revert read"
revertReadIn (w:_) c = let xs = T.input  c
                           m  = T.memory c
                           u  = T.focus . T.memory $ c
                       in  pure $ c { T.input  = BS.cons u xs
                                    , T.memory = m { T.focus = w }
                                    }

-- =============================================================== --
-- Modeling widget interfaces
-- These combinators provide an interface to manage how the widgets
-- should be rendered according to the debugger state. In particular,
-- the Program and Memory widgets have restricted display ranges to
-- allow more efficient rendering. However, these ranges need to be
-- kept up to date.

---------------------------------------------------------------------
-- Resizing the interface
-- If the terminal is resized, the view ranges of each widget need to
-- be updated.

-- Exported

resize :: Int -> Int -> T.Debugger -> T.Debugger
-- ^Resize all widgets whose display is size-dependent given a width
-- and height of the terminal.
resize w h db = db {
      T.termWidth  = w
    , T.termHeight = h
      -- The program has a border and sits on the status line so its
      -- height is three less than than the total terminal height.
    , T.progView   = resizeWgtView (h-3) (getPositionRow db) (T.progView db)
      -- The memory has a border and sits on the status line so its
      -- height is three less than than the total terminal height.
    , T.memView    = resizeWgtView (h-3) (getAddress db)     (T.memView db)
    }

-- Unexported

resizeWgtView :: Int -> Int -> T.VertViewRange -> T.VertViewRange
-- ^Given a new widget height, a current position and an old view
-- range, reset the widget's view range to enusre that the current
-- position is included in the view range.
resizeWgtView h n (n0, n1)
    | h == oldHeight              = ( n0,        n1          )
    | h > oldHeight               = ( n0',       n0' + h - 1 )
    | h < oldHeight && n < n0 + h = ( n0,        n0 + h - 1  )
    | otherwise                   = ( n - h + 1, n           )
    where oldHeight = n1 - n0 + 1
          n0'       = min 0 (n1 - h + 1)

---------------------------------------------------------------------
-- Combinators to manage what to display in a widget

updateViewByPosition :: T.Debugger -> T.Debugger
-- ^Shift the memory and program views to match the program position.
updateViewByPosition db = shiftMemView . shiftProgView row $ db
    where row = getPositionRow db

updateViewByCursor :: T.Debugger -> T.Debugger
-- ^Shift the program view to match the cursor position.
updateViewByCursor db = shiftProgView row $ db
    where row = getCursorRow db

shiftProgView :: Int -> T.Debugger -> T.Debugger
-- ^Shift range of lines to be displayed in the program UI widget.
shiftProgView pos db = db { T.progView = shiftView oldView pos }
    where oldView = T.progView db

shiftMemView :: T.Debugger -> T.Debugger
-- ^Shift range of lines to be displayed in the memory UI widget.
shiftMemView db = db { T.memView = shiftView oldView memAddress }
    where oldView    = T.memView db
          memAddress = getAddress db

shiftView :: (Int, Int) -> Int -> (Int, Int)
-- ^Shift range of lines to display in the widget (i.e., the view)
-- given an old range and a position that must be in view.
shiftView (n0,n1) n
    | n < n0    = ( n,     n + h )
    | n > n1    = ( n - h, n     )
    | otherwise = ( n0,    n1    )
    where h = n1 - n0

---------------------------------------------------------------------
-- Cycling between widgets

nextWidget :: T.WgtName -> T.WgtName
-- ^Defines a cycling order for the widgets. This is called when the
-- user presses the tab key to change the active widget.
nextWidget T.ProgramWgt = T.MemoryWgt
nextWidget T.MemoryWgt  = T.OutputWgt
nextWidget T.OutputWgt  = T.InputWgt
nextWidget T.InputWgt   = T.ProgramWgt
nextWidget T.StatusWgt  = T.StatusWgt
nextWidget T.CommandWgt = T.CommandWgt

-- =============================================================== --
-- Cursor management
-- The cursor allows a the user to move about a BF program without
-- changing the programatic or computer state. For example, the user
-- will need to move the cursor around in order to set break points.

moveCursorRight :: T.Debugger -> T.Debugger
-- ^Move the cursor position to the next statement display row. Do
-- nothing if the move is not possible.
moveCursorRight db
    | atEnd     = updateViewByCursor db
    | otherwise = updateViewByCursor $ db { T.cursor = x + 1 }
    where x     = T.cursor db
          atEnd = (== x) . subtract 1 . Vec.length . T.program $ db

moveCursorLeft :: T.Debugger -> T.Debugger
-- ^Move the cursor position to the previous statement. Do nothing if
-- the move is not possible.
moveCursorLeft db
    | x == 0    = updateViewByCursor db
    | otherwise = updateViewByCursor $ db { T.cursor = x - 1 }
    where x = T.cursor db

moveCursorUp :: T.Debugger -> T.Debugger
-- ^Move the cursor to the previous display row at the same
-- horizontal position. Do nothing if the move is not possible.
moveCursorUp db
    | x < 0     = updateViewByCursor db
    | otherwise = updateViewByCursor $ db { T.cursor = x }
    where x = T.cursor db - T.progWidth db

moveCursorDown :: T.Debugger -> T.Debugger
-- ^Move the cursor to the next display row at the same horizontal
-- postition. Do nothing if the move is not possible.
moveCursorDown db
    | atEnd     = updateViewByCursor db
    | otherwise = updateViewByCursor $ db { T.cursor = y }
    where y     = T.cursor db + T.progWidth db
          atEnd = (< y) . (subtract 1) . Vec.length . T.program $ db