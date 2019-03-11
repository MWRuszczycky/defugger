module Compiler
    (
    ) where

import qualified Data.ByteString as B
import qualified Types           as T

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
                     otherwise    -> compile p c >>= whileLoop p

compile :: T.Program -> T.Computation
compile []                   c = pure c
compile ((T.Increment  ):ps) c = increment c   >>= compile ps
compile ((T.Decrement  ):ps) c = decrement c   >>= compile ps
compile ((T.Advance    ):ps) c = advance   c   >>= compile ps
compile ((T.Backup     ):ps) c = backup    c   >>= compile ps
compile ((T.ReadIn     ):ps) c = readIn    c   >>= compile ps
compile ((T.WriteOut   ):ps) c = writeOut  c   >>= compile ps
compile ((T.WhileLoop p):ps) c = whileLoop p c >>= compile ps
compile ((T.DoNothing  ):ps) c = compile ps c
