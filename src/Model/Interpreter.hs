module Model.Interpreter
    ( -- Executing a complete BF program
      runProgram
      -- Individual computations defined by BF
    , advance
    , backup
    , decrement
    , increment
    , readIn
    , writeOut
    , whileLoop
    ) where

-- =============================================================== --
-- Combinators for interpreting BF scripts/programs                --
-- =============================================================== --

import qualified Data.ByteString as BS
import qualified Model.Types     as T
import Control.Monad                    ( foldM  )

-- =============================================================== --
-- Executing a complete BF program

runProgram :: T.Computer -> T.Program -> Either T.ErrString T.Computer
runProgram = foldM interpret

interpret :: T.Computer -> T.Statement -> Either T.ErrString T.Computer
interpret c (T.Increment  ) = increment c
interpret c (T.Decrement  ) = decrement c
interpret c (T.Advance    ) = advance   c
interpret c (T.Backup     ) = backup    c
interpret c (T.ReadIn     ) = readIn    c
interpret c (T.WriteOut   ) = writeOut  c
interpret c (T.WhileLoop p) = whileLoop p c
interpret c _               = pure c

-- =============================================================== --
-- Individual computations as defined by BF

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
