module Model.Debugger
    ( -- Executing statements
      stepForward
    , stepBackward
      -- Querying the debugger
    , getPosition
    , getAddress
    ) where

-- =============================================================== --
-- Combinators for manipulating and managing the Debugger model    --
-- =============================================================== --

import qualified Data.ByteString as BS
import qualified Model.Types     as T
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

getPosition :: T.Debugger -> Int
-- ^Provide the index of the last statement executed in the program.
-- This corresponds to the current position of the debugger that is
-- highlighted in the program UI.
getPosition db = case T.history db of
                      []    -> 0
                      (x:_) -> x

getAddress :: T.Debugger -> Int
-- ^Provide the index of the focus in the memory tape. This postioin
-- is highlighted in the memory UI.
getAddress db = let T.Tape xs _ _ = T.memory . T.computer $ db
                in  length xs

-- =============================================================== --
-- Executing statements

---------------------------------------------------------------------
-- Single steps

stepForward :: T.Debugger -> Either T.ErrString T.Debugger
-- ^Execute the next statement in the program.
stepForward db = updateHistory db >>= updateBackup >>= updateComputer

stepBackward :: T.Debugger -> Either T.ErrString T.Debugger
-- ^Revert the last statement executed in the program.
stepBackward db = revertComputer db >>= revertBackup >>= revertHistory

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
