{-# LANGUAGE OverloadedStrings #-}

module Parser
    ( parse
    ) where

import qualified Data.Text  as Tx
import qualified Data.Char  as C
import Data.List                  ( find            )
import Data.Text                  ( Text, unpack    )
import Data.Foldable              ( asum            )
import Control.Monad.State.Lazy   ( StateT (..)
                                  , evalStateT
                                  , get, guard
                                  , lift, put       )
import Control.Monad.Reader       ( ReaderT (..)
                                  , ask
                                  , runReaderT      )
import Control.Applicative        ( Alternative
                                  , (<|>)
                                  , empty
                                  , many            )
import Types                      ( Dictionary (..)
                                  , ErrString  (..)
                                  , BFParser   (..)
                                  , Program    (..)
                                  , Statement  (..)
                                  , Token      (..)
                                  , dictionary      )

---------------------------------------------------------------------
-- Entry point

parse :: Dictionary -> Text -> Either ErrString Program
parse d = evalStateT ( runReaderT program d )

parseFail :: Text -> Text -> Dictionary -> BFParser a
parseFail t0 t1 d = lift . lift . Left . go . findToken tk $ d
    where t1'              = Tx.dropWhile C.isSpace t1
          (tk, _)          = splitNext d t1'
          charNo           = 1 + Tx.length t0 - Tx.length t1'
          lineNo           = "Line " ++ show ( findLineNumber charNo t0 )
          go Nothing       = lineNo ++ ": Unrecognized token: " ++ unpack tk
          go (Just BFStop) = lineNo ++ ": Unpaired close-brace for while-loop"
          go _             = lineNo ++ ": Cannot parse while-loop"

---------------------------------------------------------------------
-- BF-Parsers

program :: BFParser Program
program = do
    t0 <- get
    p  <- many statement
    spaces
    t1 <- get
    if Tx.null t1
       then pure p
       else ask >>= parseFail t0 t1

subProgram :: BFParser Program
subProgram = do
    p <- many statement
    spaces
    token BFStop
    pure p

statement :: BFParser Statement
statement = do
    spaces
    kw <- opening
    case kw of
         BFStart   -> subProgram >>= pure . WhileLoop
         BFHash    -> skipLine   >>  pure DoNothing
         otherwise -> pure . toStatement $ kw

opening :: BFParser Token
opening = asum . map token $ [ BFGT,  BFLT,    BFPlus,  BFMinus
                             , BFDot, BFComma, BFStart, BFHash
                             ]

spaces :: BFParser ()
spaces = many ( satisfy C.isSpace ) >> pure ()

---------------------------------------------------------------------
-- Base parsers

token :: Token -> BFParser Token
token tok = do
    dict         <- ask
    possMatches  <- tryMaybe ( lookup tok ) . tokens $ dict
    (next, rest) <- splitNext dict <$> get
    guard $ elem next possMatches
    put rest >> pure tok

satisfy :: (Char -> Bool) -> BFParser Char
satisfy f = do
    (c, s) <- get >>= tryMaybe Tx.uncons
    guard $ f c
    put s >> pure c

skipLine :: BFParser ()
skipLine = do
    s <- get
    case Tx.uncons . Tx.dropWhile (/= '\n') $ s of
         Nothing      -> put Tx.empty
         Just (_, s') -> put s'

tryMaybe :: (a -> Maybe b) -> a -> BFParser b
tryMaybe f = maybe empty pure . f

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

findToken :: Text -> Dictionary -> Maybe Token
findToken t d = fmap fst . find ( elem t . snd ) . tokens $ d

findLineNumber :: Int -> Text -> Int
findLineNumber n = (+1) . Tx.length . Tx.filter (== '\n') . Tx.take n
