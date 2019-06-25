module Model.Debugger.Edit
    ( deleteStatementAtCursor
    , addStatementAtCursor
    ) where

-- =============================================================== --
-- Combinators for directly editing BF code in the debugger        --
-- =============================================================== --

import qualified Model.Types as T
import qualified Data.Vector as Vec
import qualified Data.Set    as Set
import Data.Vector                  ( (!) )
import Model.Utilities              ( vecDelete
                                    , vecInsert     )
import Model.Debugger.Query         ( getPosition
                                    , isBracket
                                    , inSameWhile   )

-- =============================================================== --
-- Common unexported helpers

shiftCrossRefs :: Int -> Int -> T.DBProgram -> T.DBProgram
-- ^Shift cross reference indices greater than or equal to i by d.
shiftCrossRefs i d = Vec.map go
    where go (T.DBOpenLoop  j) | j < i     = T.DBOpenLoop j
                               | otherwise = T.DBOpenLoop $ j + d
          go (T.DBCloseLoop j) | j < i     = T.DBCloseLoop j
                               | otherwise = T.DBCloseLoop $ j + d
          go x                 = x

shiftBreakPoints :: Int -> Int -> Set.Set Int -> Set.Set Int
-- ^Shift break point indices greater than or equal to i by d.
shiftBreakPoints i d = Set.map go
    where go j | j < i     = j
               | otherwise = j + d

---------------------------------------------------------------------
-- Error messages for BF statement deletions

startPointDelErr, endPointDelErr, evalDelErr, sameWhileDelErr :: T.ErrString
startPointDelErr = "Cannot delete: Cursor at start point"
endPointDelErr   = "Cannot delete: Cursor at end point"
evalDelErr       = "Cannot delete: Program evaluation ahead of cursor"
sameWhileDelErr  = "Cannot delete: Evaluation and cursor in same while-loop"

---------------------------------------------------------------------
-- Error messages for BF statement insertions

evalAddErr, sameWhileAddErr :: T.ErrString
evalAddErr       = "Cannot insert: Program evaluation ahead of cursor"
sameWhileAddErr  = "Cannot insert: Evaluation and cursor in same while-loop"

-- =============================================================== --
-- Deleting statements

---------------------------------------------------------------------
-- Exported

deleteStatementAtCursor :: T.Debugger -> T.Debugger
-- ^Delete the BF statement or pair of while brackets at the current
-- cursor position. Do not allow deletion if program may have
-- advanced beyond the point of deletion.
deleteStatementAtCursor db
    | cur == 0                = db { T.message = startPointDelErr }
    | cur == Vec.length p - 1 = db { T.message = endPointDelErr   }
    | pos >= cur              = db { T.message = evalDelErr       }
    | inSameWhile cur pos p   = db { T.message = sameWhileDelErr  }
    | otherwise               = deleteStatement cur db
    where pos = getPosition db
          cur = T.cursor db
          p   = T.program db

---------------------------------------------------------------------
-- Core deletion combinator that manages updating all other parts
-- of the debugger in response to a deletion edit. This includes
-- updating break points and while loop bracket indexing.

deleteStatement :: Int -> T.Debugger -> T.Debugger
-- ^Delete the statement at position i in the BF script.
deleteStatement i db =
    let p = T.program db
        b = T.breaks  db
    in  case p ! i of
             (T.DBOpenLoop  j) ->
                db { T.program = shiftCrossRefs i (-1) . vecDelete i
                                 . shiftCrossRefs j (-1) . vecDelete j $ p
                   , T.breaks  = shiftBreakPoints i (-1)
                                 . shiftBreakPoints j (-1)
                                 . Set.delete i . Set.delete j $ b
                   , T.cursor  = i - 1 }
             (T.DBCloseLoop j) ->
                db { T.program = shiftCrossRefs j (-1) . vecDelete j
                                 . shiftCrossRefs i (-1) . vecDelete i $ p
                   , T.breaks  = shiftBreakPoints j (-1)
                                 . shiftBreakPoints i (-1)
                                 . Set.delete i . Set.delete j $ b
                   , T.cursor  = i - 2 }
             _                 ->
                db { T.program = shiftCrossRefs i (-1) . vecDelete i $ p
                   , T.breaks  = shiftBreakPoints i (-1) . Set.delete i $ b
                   , T.cursor  = i - 1 }

-- =============================================================== --
-- Inserting statements

---------------------------------------------------------------------
-- Exported

addStatementAtCursor :: T.DebugStatement -> T.Debugger -> T.Debugger
-- ^Add a given BF statement x to the current cursor position. Do
-- not allow insertion when the program evaluation may have advanced
-- beyond the point of insertion.
addStatementAtCursor x db
    | cur == 0 && pos == 0  = addStatementAtCursor x $ db { T.cursor = 1 }
    | pos >= cur            = db { T.message = evalAddErr      }
    | inSameWhile cur pos p = db { T.message = sameWhileAddErr }
    | otherwise             = addStatement cur x db
    where pos = getPosition db
          cur = T.cursor db
          p   = T.program db

---------------------------------------------------------------------
-- Core insertion combinator that manages updating all other parts
-- of the debugger in response to an insertion edit. This includes
-- updating break points and while loop bracket indexing.

addStatement :: Int -> T.DebugStatement -> T.Debugger -> T.Debugger
-- ^Insert BF statement x at position i in the BF program.
addStatement i x db
    | isBracket x = db { T.program = vecInsert i op . vecInsert i cl
                                     . shiftCrossRefs i 2 $ p
                       , T.breaks  = shiftBreakPoints i 2 $ b
                       , T.cursor  = i + 1 }
    | otherwise   = db { T.program = vecInsert i x . shiftCrossRefs i 1 $ p
                       , T.breaks  = shiftBreakPoints i 1 b
                       , T.cursor  = i + 1 }
    where p  = T.program db
          b  = T.breaks  db
          cl = T.DBCloseLoop   i
          op = T.DBOpenLoop  $ i + 1
