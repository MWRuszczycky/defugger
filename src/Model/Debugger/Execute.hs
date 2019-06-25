{-# LANGUAGE BangPatterns #-}

module Model.Debugger.Execute
    ( stepForward
    , stepBackward
    , jumpForward
    , jumpBackward
    ) where

-- =============================================================== --
-- Combinators for modeling program execution in the debugger      --
-- =============================================================== --

import qualified Data.ByteString as BS
import qualified Model.Types     as T
import qualified Data.Set        as Set
import Data.Word                        ( Word8                 )
import Data.Vector                      ( (!)                   )
import Model.Debugger.Query             ( getPosition           )
import Model.Debugger.Widgets           ( updateViewByPosition  )
import Model.Interpreter                ( advance
                                        , backup
                                        , decrement
                                        , increment
                                        , readIn
                                        , writeOut              )

-- =============================================================== --
-- Exported combinators, which provide the interface

---------------------------------------------------------------------
-- Advancing execution of the BF program

stepForward :: T.Debugger -> T.Debugger
-- ^Execute the next BF program statement.
stepForward db = either setMsg id . executeNextStatement $ db
    where setMsg x = db { T.message = x }

jumpForward :: T.Debugger -> T.Debugger
-- ^Execute all BF statements up and including the next break point.
jumpForward db = either setMsg id . executeToNextBreak $ db
    where setMsg x = db { T.message = x }

---------------------------------------------------------------------
-- Reverting execution of the BF program

stepBackward :: T.Debugger -> T.Debugger
-- ^Revert the last BF program statement that was executed.
stepBackward db = either setMsg id . revertLastStatement $ db
    where setMsg x = db { T.message = x }

jumpBackward :: T.Debugger -> T.Debugger
-- ^Revert all previous BF statements following the last break point.
jumpBackward db = either setMsg id . revertToLastBreak $ db
    where setMsg x = db { T.message = x }

-- =============================================================== --
-- Managing the Debugger state in response to BF program exection
-- and reversion. These combinators should not be exported.

executeNextStatement :: T.Debugger -> Either T.ErrString T.Debugger
-- ^Execute the next statement in the program if possible.
executeNextStatement db = advanceHistory db
                          >>= advanceBackup
                          >>= advanceComputer
                          >>= pure . updateViewByPosition

revertLastStatement :: T.Debugger -> Either T.ErrString T.Debugger
-- ^Revert the last statement executed in the program if possible.
revertLastStatement db = revertComputer db
                         >>= revertBackup
                         >>= revertHistory
                         >>= pure . updateViewByPosition

executeToNextBreak :: T.Debugger -> Either T.ErrString T.Debugger
-- ^Execute all statements up to and including the next break point.
executeToNextBreak db = executeNextStatement db >>= go
    where go !db' | Set.member (getPosition db') (T.breaks db') = pure db'
                  | otherwise = executeToNextBreak db'

revertToLastBreak :: T.Debugger -> Either T.ErrString T.Debugger
-- ^Revert all statements after the last break point.
revertToLastBreak db = revertLastStatement db >>= go
    where go !db' | Set.member (getPosition db') (T.breaks db') = pure db'
                  | otherwise = revertToLastBreak db'

-- =============================================================== --
-- Combinators for managing the update of each component of the
-- debugger in response to programmatic advancement or reversion.
-- These combinators should not be exported.

---------------------------------------------------------------------
-- Computer

advanceComputer :: T.Debugger -> Either T.ErrString T.Debugger
advanceComputer db = ( \ x -> db { T.computer = x } ) <$> c'
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

---------------------------------------------------------------------
-- History
-- The history tracks what statements in the program have been so far
-- executed and in what order. This allows back-tracking.

advanceHistory :: T.Debugger -> Either T.ErrString T.Debugger
advanceHistory db = pure $ db { T.history = h' }
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
-- Backup Memory
-- The backup memory is used to track what has been read from input
-- to the computer memory. This is necessary to ensure the orginal
-- memory state can be recovered when reverting statements.

advanceBackup :: T.Debugger -> Either T.ErrString T.Debugger
advanceBackup db =
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
