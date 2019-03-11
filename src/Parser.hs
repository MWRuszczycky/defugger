module Parser
    ( parse
    ) where

import qualified Types      as T
import Control.Applicative       ( Alternative
                                 , (<|>)
                                 , many, empty   )

---------------------------------------------------------------------

parse :: T.BFScript -> Maybe T.Program
parse = maybe Nothing (Just . snd) . T.runParser program

program :: T.Parser T.Token T.Program
program = do
    p <- many statement
    endOfInput
    pure p

statement :: T.Parser T.Token T.Statement
statement = do
    kw <- action <|> start
    case kw of
         T.BFStart -> routine >>= pure . T.WhileLoop
         otherwise -> pure . toStatement $ kw

routine :: T.Parser T.Token T.Program
routine = do
    p <- many statement
    stop >> pure p

stop :: T.Parser T.Token T.Token
stop = satisfy (== T.BFStop)

start :: T.Parser T.Token T.Token
start = satisfy (== T.BFStart)

action :: T.Parser T.Token T.Token
action = satisfy $ flip elem [ T.BFGT, T.BFLT, T.BFPlus
                             , T.BFMinus, T.BFDot, T.BFComma ]

endOfInput :: T.Parser T.Token ()
endOfInput = T.Parser $ \ s -> if null s then Just ([], ()) else Nothing

satisfy :: (a -> Bool) -> T.Parser a a
satisfy f = T.Parser $ \ s ->
    case s of
         []   -> Nothing
         x:xs -> if f x then Just (xs, x) else Nothing

toStatement :: T.Token -> T.Statement
toStatement T.BFGT    = T.Advance
toStatement T.BFLT    = T.Backup
toStatement T.BFPlus  = T.Increment
toStatement T.BFMinus = T.Decrement
toStatement T.BFComma = T.ReadIn
toStatement T.BFDot   = T.WriteOut
toStatement _         = T.Skip

parseScript :: String -> T.BFScript
parseScript [] = []
parseScript (x:xs) = let rest = parseScript xs
                     in  case x of
                              '>' -> T.BFGT    : rest
                              '<' -> T.BFLT    : rest
                              '+' -> T.BFPlus  : rest
                              '-' -> T.BFMinus : rest
                              '.' -> T.BFDot   : rest
                              ',' -> T.BFComma : rest
                              '[' -> T.BFStart : rest
                              ']' -> T.BFStop  : rest
                              _   -> rest
