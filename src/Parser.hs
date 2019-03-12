{-# LANGUAGE OverloadedStrings #-}

module Parser
    ( parse
    ) where

import qualified Data.Text  as Tx
import qualified Data.Char  as C
import Data.Foldable                ( asum          )
import Control.Monad.State.Lazy     ( StateT  (..)
                                    , get, put
                                    , guard
                                    , evalStateT    )
import Control.Monad.Reader         ( ReaderT (..)
                                    , ask
                                    , runReaderT
                                    , lift          )
import Types
import Data.List                    ( maximumBy
                                    , minimumBy
                                    , find          )
import Data.Text                    ( Text          )
import Control.Applicative          ( Alternative
                                    , (<|>)
                                    , liftA2
                                    , many, empty   )

---------------------------------------------------------------------
-- Entry point

parse :: Dictionary -> Text -> Maybe Program
parse d = evalStateT ( runReaderT program d )

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
    token BFStop
    pure p

statement :: BFParser Statement
statement = do
    many ( satisfy C.isSpace )
    kw <- opening
    case kw of
         BFStart -> subProgram >>= pure . WhileLoop
         BFHash  -> skipLine >> pure DoNothing
         otherwise -> pure . toStatement $ kw

opening :: BFParser Token
opening = asum . map token $ [ BFGT,  BFLT,    BFPlus,  BFMinus
                             , BFDot, BFComma, BFStart, BFHash
                             ]

endOfInput :: BFParser ()
endOfInput = get >>= guard . Tx.null

---------------------------------------------------------------------
-- Base parsers

token :: Token -> BFParser Token
token tok = do
    dict         <- ask
    possMatches  <- lift . lift . lookup tok . tokens $ dict
    (next, rest) <- splitNext dict <$> get
    guard $ elem next possMatches
    put rest >> pure tok

satisfy :: (Char -> Bool) -> BFParser Char
satisfy f = do
    (c, s) <- get >>= lift . lift . Tx.uncons
    guard $ f c
    put s >> pure c

skipLine :: BFParser ()
skipLine = do
    s <- get
    case Tx.uncons . Tx.dropWhile (/= '\n') $ s of
         Nothing      -> put Tx.empty
         Just (_, s') -> put s'

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

runTest = parse bfDict testProg

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
