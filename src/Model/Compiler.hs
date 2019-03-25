module Model.Compiler
    ( runProgram
    , advance
    , backup
    , increment
    , decrement
    , readIn
    , writeOut
    , whileLoop
    , stepForward
    , stepBackward
    , getPosition
    ) where

import qualified Data.ByteString as BS
import qualified Model.Types     as T
import Data.Word                        ( Word8  )
import Control.Monad                    ( foldM  )
import Data.Vector                      ( (!)    )

---------------------------------------------------------------------
-- Running a program directly

runProgram :: T.Computer -> T.Program -> Either T.ErrString T.Computer
runProgram = foldM compile

compile :: T.Computer -> T.Statement -> Either T.ErrString T.Computer
compile c (T.Increment  ) = increment c
compile c (T.Decrement  ) = decrement c
compile c (T.Advance    ) = advance   c
compile c (T.Backup     ) = backup    c
compile c (T.ReadIn     ) = readIn    c
compile c (T.WriteOut   ) = writeOut  c
compile c (T.WhileLoop p) = whileLoop p c
compile c (T.DoNothing  ) = pure c

advance :: T.Computation
advance c = case T.memory c of
                 T.Tape xs u []     -> Right c { T.memory = T.Tape (u:xs) 0 [] }
                 T.Tape xs u (y:ys) -> Right c { T.memory = T.Tape (u:xs) y ys }

backup :: T.Computation
backup c = case T.memory c of
                T.Tape []     _ _  -> Left "Invalid access ahead of start"
                T.Tape (x:xs) 0 [] -> Right c { T.memory = T.Tape xs x []     }
                T.Tape (x:xs) u ys -> Right c { T.memory = T.Tape xs x (u:ys) }

increment :: T.Computation
increment c = let (T.Tape xs u ys) = T.memory c
              in  Right c { T.memory = T.Tape xs (u + 1) ys }

decrement :: T.Computation
decrement c = let (T.Tape xs u ys) = T.memory c
              in  Right c { T.memory = T.Tape xs (u - 1) ys }

readIn :: T.Computation
readIn c = let (T.Tape xs _ ys) = T.memory c
           in  case BS.uncons . T.input $ c of
                    Nothing      -> Left "Attempt to read past end of input"
                    Just (b, bs) -> Right c { T.memory = T.Tape xs b ys
                                            , T.input  = bs
                                            }

writeOut :: T.Computation
writeOut c = let (T.Tape _ u _) = T.memory c
             in  Right c { T.output = BS.snoc ( T.output c ) u }

whileLoop :: T.Program -> T.Computation
whileLoop p c = case T.memory c of
                     T.Tape _ 0 _ -> pure c
                     _            -> runProgram c p >>= whileLoop p

-- =============================================================== --
-- Running a program via the debugger

stepForward :: T.Debugger -> Either T.ErrString T.Debugger
stepForward db = updateHistory db >>= updateBackup >>= updateComputer

stepBackward :: T.Debugger -> Either T.ErrString T.Debugger
stepBackward db = revertComputer db >>= revertBackup >>= revertHistory

---------------------------------------------------------------------
-- Helpers

getPosition :: T.Debugger -> Int
getPosition db = case T.history db of
                      []    -> 0
                      (x:_) -> x

---------------------------------------------------------------------
-- Debug history management

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
-- Debug backup read-memory management

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
-- Debug computer management

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
