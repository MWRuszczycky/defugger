{-# LANGUAGE OverloadedStrings #-}

module Parser
    ( parse
    ) where

import qualified Data.Text  as Tx
import qualified Data.Char  as C
import Control.Monad.Reader         ( ReaderT (..)
                                    , runReaderT
                                    , lift          )
import Types
import Data.List                    ( maximumBy
                                    , minimumBy
                                    , find          )
import Data.Text                    ( Text          )
import Control.Applicative          ( Alternative
                                    , (<|>)
                                    , many, empty   )

---------------------------------------------------------------------
-- Entry point

parse :: Dictionary -> Text -> Maybe Program
parse d t = maybe Nothing ( \ (_,x) -> Just x )
            . runParser ( runReaderT program d ) $ t

---------------------------------------------------------------------
-- Parsers

program :: BFParser Program
program = do
    p <- many statement
    endOfInput
    pure p

subProgram :: BFParser Program
subProgram = do
    p <- many statement
    stop >> pure p

statement :: BFParser Statement
statement = do
    spaces
    kw <- opening
    case kw of
         BFStart -> subProgram >>= pure . WhileLoop
         BFHash  -> skipLine >> pure DoNothing
         otherwise -> pure . toStatement $ kw

opening :: BFParser Token
opening = choice . map token $ [ BFGT,  BFLT,    BFPlus,  BFMinus
                               , BFDot, BFComma, BFStart, BFHash
                               ]

stop :: BFParser Token
stop = token BFStop

start :: BFParser Token
start = token BFStart

spaces :: BFParser ()
spaces = many ( satisfy C.isSpace ) >> pure ()

---------------------------------------------------------------------
-- Base parsers

choice :: [BFParser a] -> BFParser a
choice []     = empty
choice (x:[]) = x
choice (x:xs) = x <|> choice xs

endOfInput :: BFParser ()
endOfInput = lift . Parser $ \ s ->
    if Tx.null s
       then Just (Tx.empty, ())
       else Nothing

token :: Token -> BFParser Token
token tok = ReaderT $ \ d -> Parser $ \ s ->
    do -- This is a Maybe monad
    let (next, rest) = splitNext d s
    strings <- lookup tok . tokens $ d
    if elem next strings
       then Just (rest, tok)
       else Nothing

satisfy :: (Char -> Bool) -> BFParser Char
satisfy f = lift . Parser $ \ s ->
    do -- This is a Maybe monad
    (t, ts) <- Tx.uncons s
    if f t
       then Just (ts, t)
       else Nothing

skipLine :: BFParser ()
skipLine = lift . Parser $ \ s ->
    case Tx.uncons . Tx.dropWhile (/= '\n') $ s of
         Nothing      -> Just (Tx.empty, ())
         Just (_, s') -> Just (s',       ())

---------------------------------------------------------------------
-- Helpers

toStatement :: Token -> Statement
toStatement BFGT    = Advance
toStatement BFLT    = Backup
toStatement BFPlus  = Increment
toStatement BFMinus = Decrement
toStatement BFComma = ReadIn
toStatement BFDot   = WriteOut
toStatement _       = DoNothing

splitNext :: Dictionary -> Text -> (Text, Text)
splitNext d
    | isShort d = Tx.splitAt 1
    | otherwise = Tx.break C.isSpace

---------------------------------------------------------------------
-- For testing

bfDict :: Dictionary
bfDict =  toDictionary [ ( BFGT,    [">"] )
                       , ( BFLT,    ["<"] )
                       , ( BFPlus,  ["+"] )
                       , ( BFMinus, ["-"] )
                       , ( BFDot,   ["."] )
                       , ( BFComma, [","] )
                       , ( BFStart, ["["] )
                       , ( BFStop,  ["]"] )
                       , ( BFHash,  ["#"] )
                       ]

testProg :: Text
testProg = "++> ++[<+>-]-->> # a comment \n <<[<>--++[+<->+]]->+"
