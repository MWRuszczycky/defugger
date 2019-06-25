module Model.Debugger.Query
    ( -- Debugger program, memory & script location queries
      getAddress
    , getPosition
    , getPositionRow
    , getCursorRow
    , isAtStart
    , isAtEnd
      -- Queries for While (i.e., '[' and ']') loops
    , isBracket
    , getOuterWhile
    , inSameWhile
    ) where

-- =============================================================== --
-- Combinators for querying debugger BF programmatic state         --
-- =============================================================== --

import qualified Model.Types as T
import qualified Data.Vector as Vec
import Data.Vector                ( (!)       )

-- =============================================================== --
-- Debugger program, memory & script location queries
-- Address:  Where where the memory is focused.
-- Position: Last programatic BF statement to be executed.
-- Cursor:   Programmatic BF statement highlighted by the cursor.

---------------------------------------------------------------------
-- Memory address location

getAddress :: T.Debugger -> Int
getAddress db = let T.Tape xs _ _ = T.memory . T.computer $ db
                in  length xs

---------------------------------------------------------------------
-- Executed BF statement location

getPosition :: T.Debugger -> Int
getPosition db = case T.history db of
                      []    -> 0
                      (x:_) -> x

getPositionRow :: T.Debugger -> Int
-- ^Row in the program widget window where the program is currently
-- stopped (i.e., where the last statement executed is displayed).
getPositionRow db = quot (getPosition db) (T.progWidth db)

isAtStart :: T.Debugger -> Bool
-- ^Is the program execution at the start position.
isAtStart db = case T.program db ! getPosition db of
                    T.DBStart -> True
                    _         -> False

isAtEnd :: T.Debugger -> Bool
-- ^Has program execution completed (i.e., at the stop position).
isAtEnd db = case T.program db ! getPosition db of
                    T.DBEnd -> True
                    _       -> False

---------------------------------------------------------------------
-- User highlighted cursor location in the BF script

-- The cursor position can be accessed directly using the
-- Model.Types.cursor
-- accessor in Model.Types.Debugger.

getCursorRow :: T.Debugger -> Int
-- ^Row in the program widget window where the cursor can be found.
getCursorRow db = quot (T.cursor db) (T.progWidth db)

-- =============================================================== --
-- Queries for While (i.e., '[' and ']') loops

isBracket :: T.DebugStatement -> Bool
-- ^Is a statement a while-loop bracked either opening or closing.
isBracket (T.DBOpenLoop  _ ) = True
isBracket (T.DBCloseLoop _ ) = True
isBracket _                  = False

inSameWhile :: Int -> Int -> T.DBProgram -> Bool
-- ^Are the i-th and j-th statements within a common while loop.
inSameWhile i j p = case ( getOuterWhile i p, getOuterWhile j p) of
                         (Nothing, _      ) -> False
                         (_,       Nothing) -> False
                         (Just x,  Just y ) -> x == y

getOuterWhile :: Int -> T.DBProgram -> Maybe (Int, Int)
-- ^Indices of the outermost while-loop brackets about the n-th BF
-- statement in a program p. If n is itself a bracket, then it is
-- still treated as being within the bracket.
getOuterWhile n p
    | n < 1     = Nothing
    | 1 >= m    = Nothing
    | null ws   = if isBracket (p ! n) then Just (i', j') else Nothing
    | otherwise = Just (i, j)
    where m                  = Vec.length p - n
          ws                 = Vec.foldl' go [] . Vec.slice n m $ p
          (T.DBCloseLoop i ) = head ws -- Found an outer loop
          (T.DBOpenLoop  j ) = p ! i
          (T.DBOpenLoop  j') = p ! n   -- Starts at the opening of outer loop
          (T.DBCloseLoop i') = p ! j'
          go (T.DBOpenLoop _:xs) (T.DBCloseLoop _) = xs
          go xs                  (T.DBCloseLoop k) = T.DBCloseLoop k : xs
          go xs                  (T.DBOpenLoop  k) = T.DBOpenLoop k  : xs
          go xs                  _                 = xs
