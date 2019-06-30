module Model.Debugger.Widgets
    ( -- Managing widget dimensions for the debugger display
      resize
    , nextWidget
      -- Combinators to manage what to display in the subwidgets
    , updateViewByPosition
    , updateViewByCursor
    , changeFormat
    , noMessage
      -- Cursor management
    , moveCursorRight
    , moveCursorLeft
    , moveCursorUp
    , moveCursorDown
      -- Break point management
    , setBreakPoint
    , unsetBreakPoint
    , unsetAllBreakPoints
    ) where

-- =============================================================== --
-- Modeling widget interfaces                                      --
-- These combinators are used to update the debugger so that it    --
-- be properly rendered by the Brick inteface. In particular, the  --
-- Program and Memory widgets have restricted display ranges to    --
-- allow more efficient rendering, However, these ranges need to   --
-- be kept up to date.                                             --
-- =============================================================== --

import qualified Model.Types as T
import qualified Data.Vector as Vec
import qualified Data.Set    as Set
import Model.Debugger.Query         ( getPositionRow
                                    , getAddress
                                    , getPositionRow
                                    , getCursorRow    )

-- =============================================================== --
-- Managing widget dimensions for the debugger dislplay

resize :: Int -> Int -> T.Debugger -> T.Debugger
-- ^Resize all widget components given a new terminal width & height.
resize w h db = db {
      T.termWidth  = w
    , T.termHeight = h
      -- The program has a border and title and sits on the status
      -- line so its height is four less than the terminal height.
    , T.progView   = resizeWgtView (h-4) (getPositionRow db) (T.progView db)
      -- The memory has a border and title and sits on the status
      -- line so its height is four less than the terminal height.
    , T.memView    = resizeWgtView (h-4) (getAddress db)     (T.memView db)
    }

---------------------------------------------------------------------
-- Unexported

resizeWgtView :: Int -> Int -> T.VertViewRange -> T.VertViewRange
-- ^Given a new widget height, a current position and an old view
-- range, reset the widget's view range to ensure that the current
-- position is included in the view range.
resizeWgtView h n (n0, n1)
    | h == oldHeight              = ( n0,        n1          )
    | h > oldHeight               = ( n0',       n0' + h - 1 )
    | h < oldHeight && n < n0 + h = ( n0,        n0 + h - 1  )
    | otherwise                   = ( n - h + 1, n           )
    where oldHeight = n1 - n0 + 1
          n0'       = min 0 (n1 - h + 1)

-- =============================================================== --
-- Combinators to manage what to display in the subwidgets

---------------------------------------------------------------------
-- Shifting line ranges of memory or BF script to display

updateViewByPosition :: T.Debugger -> T.Debugger
-- ^Shift the memory and program views to match the program position.
updateViewByPosition db = shiftMemView memRow . shiftProgView progRow $ db
    where progRow = getPositionRow db
          memRow  = getAddress db

updateViewByCursor :: T.Debugger -> T.Debugger
-- ^Shift the program view to match the cursor position.
updateViewByCursor db = shiftProgView row $ db
    where row = getCursorRow db

---------------------------------------------------------------------
-- Display formats and messaging

changeFormat :: T.DataFormat -> T.Debugger -> T.Debugger
-- ^Change the data format of the currentl focused widget.
changeFormat fmt db = case T.wgtFocus db of
                           T.OutputWgt -> db { T.outFormat = fmt }
                           T.InputWgt  -> db { T.inFormat  = fmt }
                           _           -> db

noMessage :: T.Debugger -> T.Debugger
noMessage db = db { T.message = "" }

---------------------------------------------------------------------
-- Cycling between active subwidgets in response to changes in focus

nextWidget :: T.WgtName -> T.WgtName
nextWidget T.ProgramWgt = T.MemoryWgt
nextWidget T.MemoryWgt  = T.OutputWgt
nextWidget T.OutputWgt  = T.InputWgt
nextWidget T.InputWgt   = T.ProgramWgt
nextWidget T.StatusWgt  = T.StatusWgt
nextWidget T.CommandWgt = T.CommandWgt

---------------------------------------------------------------------
-- Helpers

shiftView :: (Int, Int) -> Int -> (Int, Int)
-- ^Shift the range of lines to display in the widget given the
-- previous range of lines displayed and a line that must be in view.
shiftView (n0,n1) n
    | n < n0    = ( n,     n + h )
    | n > n1    = ( n - h, n     )
    | otherwise = ( n0,    n1    )
    where h = n1 - n0

shiftProgView :: Int -> T.Debugger -> T.Debugger
-- ^Shift range of lines to be displayed in the program UI widget.
shiftProgView row db = db { T.progView = shiftView oldView row }
    where oldView = T.progView db

shiftMemView :: Int -> T.Debugger -> T.Debugger
-- ^Shift range of lines to be displayed in the memory UI widget.
shiftMemView row db = db { T.memView = shiftView oldView row }
    where oldView = T.memView db

-- =============================================================== --
-- Cursor management
-- The cursor allows a the user to move about a BF program without
-- changing the programatic or computer state. For example, the user
-- will need to move the cursor around in order to set break points.

moveCursorRight :: T.Debugger -> T.Debugger
-- ^Move the cursor to the next column of BF statements.
moveCursorRight db
    | atEnd     = updateViewByCursor db
    | otherwise = updateViewByCursor $ db { T.cursor = x + 1 }
    where x     = T.cursor db
          atEnd = (== x) . subtract 1 . Vec.length . T.program $ db

moveCursorLeft :: T.Debugger -> T.Debugger
-- ^Move the cursor to the previous column of BF statements.
moveCursorLeft db
    | x == 0    = updateViewByCursor db
    | otherwise = updateViewByCursor $ db { T.cursor = x - 1 }
    where x = T.cursor db

moveCursorUp :: T.Debugger -> T.Debugger
-- ^Move the cursor to the previous row at of BF statements.
moveCursorUp db
    | x < 0     = updateViewByCursor db
    | otherwise = updateViewByCursor $ db { T.cursor = x }
    where x = T.cursor db - T.progWidth db

moveCursorDown :: T.Debugger -> T.Debugger
-- ^Move the cursor to the next row of BF statements.
moveCursorDown db
    | atEnd     = updateViewByCursor db
    | otherwise = updateViewByCursor $ db { T.cursor = y }
    where y     = T.cursor db + T.progWidth db
          atEnd = (< y) . (subtract 1) . Vec.length . T.program $ db

-- =============================================================== --
-- Break point management

setBreakPoint :: T.Debugger -> T.Debugger
-- ^Set a break point at the current cursor position.
setBreakPoint db = db { T.breaks = Set.insert n bs }
    where n  = T.cursor db
          bs = T.breaks db

unsetBreakPoint :: T.Debugger -> T.Debugger
-- ^Unset any break point at the current cursor position.
unsetBreakPoint db
    | n == 0       = db
    | n == len - 1 = db
    | otherwise    = db { T.breaks = Set.delete n bs }
    where n   = T.cursor db
          bs  = T.breaks db
          len = Vec.length . T.program $ db

unsetAllBreakPoints :: T.Debugger -> T.Debugger
unsetAllBreakPoints db = db { T.breaks = Set.fromList [ 0, len - 1 ] }
    where len = Vec.length . T.program $ db
