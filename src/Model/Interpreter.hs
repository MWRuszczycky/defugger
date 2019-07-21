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
import Control.Monad.Cont               ( Cont, callCC
                                        , runCont, foldM )

-- =============================================================== --
-- The interpreter runs in a continuation monad so that it can break
-- in two different ways:
--   1. Error occurs (e.g., attempting to read unavailable memory.)
--      This yields a Left T.ErrString result.
--   2. A user-inserted break point is encountered.
--      This yields a Right T.Computer result, just as if the script
--      had run to the end.
-- Getting breaks is why we need the Cont r monad instead of just the
-- Either T.ErrString monad. The previous implementation did use the
-- Either T.ErrString monad to just handle errors and not breaks.
-- This was actually pretty fast completing the Mandelbrot.bf script
-- in about 5.5 min. Using the Cont r monad appears to add some
-- additional overhead so that the Mandelbrot.bf script takes about
-- 7.0 min. So, about 20% slower---not too terrible and there do not
-- appear to be any space leaks. There may be a better, faster
-- implementation using a monad transformer stack that takes
-- advantage of the Either T.ErrString monad as well as the Cont r
-- monad, but this works reasonably well for now.

-- |A ComputationResult is the result of executing a BF statement.
type ComputationResult = Either T.ErrString T.Computer

-- |A Computation is a suspended computation that will supply a
-- ComputationResult accessible via a continuation.
type Computation r = Cont r ComputationResult

-- |An Escape is a continuation used to short-circuit a Computation
-- in the case that either a break point or an error is encountered.
type Escape r b = ComputationResult -> Cont r b

-- =============================================================== --
-- Executing a complete BF program

runProgram :: T.Computer -> T.Program -> ComputationResult
runProgram c p = runCont (compute c p) id

compute :: T.Computer -> T.Program -> Computation r
compute c p = callCC $ \ esc -> foldM (interpret esc) (Right c) p

interpret :: Escape r b -> ComputationResult -> T.Statement -> Computation r
interpret esc (Left  e) _               = esc ( Left e ) >> pure ( Left e )
interpret _   (Right c) (T.Increment  ) = pure . increment   $ c
interpret _   (Right c) (T.Decrement  ) = pure . decrement   $ c
interpret _   (Right c) (T.Advance    ) = pure . advance     $ c
interpret _   (Right c) (T.Backup     ) = pure . backup      $ c
interpret _   (Right c) (T.ReadIn     ) = pure . readIn      $ c
interpret _   (Right c) (T.WriteOut   ) = pure . writeOut    $ c
interpret _   (Right c) (T.WhileLoop p) = pure . whileLoop p $ c
interpret esc c         (T.Break      ) = esc c >> pure c
interpret _   c         _               = pure c

-- =============================================================== --
-- Generating Compuation Results according to BF statements.
-- Note that these functions are defined entirely in the
-- Either T.ErrorString monad so that they can be used elsewhere
-- without having to deal with continuations.

advance :: T.Computer -> ComputationResult
advance c = case T.memory c of
                 T.Tape xs u []     -> Right c { T.memory = T.Tape (u:xs) 0 [] }
                 T.Tape xs u (y:ys) -> Right c { T.memory = T.Tape (u:xs) y ys }

backup :: T.Computer -> ComputationResult
backup c = case T.memory c of
                T.Tape []     _ _  -> Left "Invalid access ahead of start"
                T.Tape (x:xs) 0 [] -> Right c { T.memory = T.Tape xs x []     }
                T.Tape (x:xs) u ys -> Right c { T.memory = T.Tape xs x (u:ys) }

increment :: T.Computer -> ComputationResult
increment c = let (T.Tape xs u ys) = T.memory c
              in  Right c { T.memory = T.Tape xs (u + 1) ys }

decrement :: T.Computer -> ComputationResult
decrement c = let (T.Tape xs u ys) = T.memory c
              in  Right c { T.memory = T.Tape xs (u - 1) ys }

readIn :: T.Computer -> ComputationResult
readIn c = let (T.Tape xs _ ys) = T.memory c
           in  case BS.uncons . T.input $ c of
                    Nothing      -> Left "Attempt to read past end of input"
                    Just (b, bs) -> Right c { T.memory = T.Tape xs b ys
                                            , T.input  = bs
                                            }

writeOut :: T.Computer -> ComputationResult
writeOut c = let (T.Tape _ u _) = T.memory c
             in  Right c { T.output = BS.snoc ( T.output c ) u }

whileLoop :: T.Program -> T.Computer -> ComputationResult
whileLoop p c = case T.memory c of
                     T.Tape _ 0 _ -> pure c
                     _            -> runProgram c p >>= whileLoop p
