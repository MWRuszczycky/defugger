module Model.Interpreter
    ( -- Executing a complete BF program
      runProgram
    , runProgramFast
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
-- There are two versions of the interpreter:
-- 1. runProgram
--      Uses a Cont r monad and can catch errors as well as break
--      points in the program. Using the Cont r monad adds additional
--      overhead, so that this interpreter runs about 50% slower than
--      runProgramFast but is still much faster than the debugger.
--      This should complete the mandlebrot.bf script in about 8 min.
-- 2. runProgramFast
--      Uses only the Either T.ErrString monad, so it runs faster
--      than runProgram. This should complete the mandelbrot.bf
--      script in about 5.5 min. This is also the basis for all the
--      exported computation functions.

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

---------------------------------------------------------------------
-- Somewhat slower versions using continuations that allow breaks

runProgram :: T.Computer -> T.Program -> ComputationResult
-- ^Slower version of the interpreter that can catch breaks.
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
interpret esc etComp    (T.WhileLoop p) = whileLoopCont esc p etComp
interpret esc etComp    (T.Break      ) = esc etComp >> pure etComp
interpret _   etComp    _               = pure etComp

whileLoopCont :: Escape r b -> T.Program -> ComputationResult -> Computation r
-- ^Need a separate while-loop evaluator for the continuation monad
-- so that the escape continuation is passed into the subprograms.
-- Without this, the program hangs on break points in while-loops.
whileLoopCont esc _ (Left  e) = esc ( Left e ) >> pure ( Left e )
whileLoopCont esc p (Right c) =
    case T.memory c of
         T.Tape _ 0 _ -> pure . Right $ c
         _            -> foldM (interpret esc) (Right c) p
                         >>= whileLoopCont esc p

---------------------------------------------------------------------
-- Faster version that ignores breaks but catches errors

runProgramFast :: T.Computer -> T.Program -> ComputationResult
-- ^Faster version of the interpreter that cannot catch breaks.
runProgramFast = foldM interpretFast

interpretFast :: T.Computer -> T.Statement -> ComputationResult
interpretFast c (T.Increment  ) = increment c
interpretFast c (T.Decrement  ) = decrement c
interpretFast c (T.Advance    ) = advance   c
interpretFast c (T.Backup     ) = backup    c
interpretFast c (T.ReadIn     ) = readIn    c
interpretFast c (T.WriteOut   ) = writeOut  c
interpretFast c (T.WhileLoop p) = whileLoop p c
interpretFast c _               = pure c

-- =============================================================== --
-- Generating Computation Results according to BF statements.
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
                     _            -> runProgramFast c p
                                     >>= whileLoop p
