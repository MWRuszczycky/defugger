module Compiler
    ( runProgram
    ) where

import qualified Data.ByteString as B
import qualified Types           as T
import Control.Monad                    ( foldM )

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
                T.Tape []     _ _  -> Left "Invalid access ahead of start."
                T.Tape (x:xs) u ys -> Right c { T.memory = T.Tape xs x (u:ys) }

increment :: T.Computation
increment c = let (T.Tape xs u ys) = T.memory c
              in  Right c { T.memory = T.Tape xs (u + 1) ys }

decrement :: T.Computation
decrement c = let (T.Tape xs u ys) = T.memory c
              in  Right c { T.memory = T.Tape xs (u - 1) ys }

readIn :: T.Computation
readIn c = let (T.Tape xs _ ys) = T.memory c
           in  case B.uncons . T.input $ c of
                    Nothing      -> Left "At end of T.input"
                    Just (b, bs) -> Right c { T.memory = T.Tape xs b ys
                                            , T.input  = bs
                                            }

writeOut :: T.Computation
writeOut c = let (T.Tape _ u _) = T.memory c
             in  Right c { T.output = B.snoc ( T.output c ) u }

whileLoop :: T.Program -> T.Computation
whileLoop p c = case T.memory c of
                     T.Tape _ 0 _ -> pure c
                     otherwise    -> runProgram c p >>= whileLoop p
