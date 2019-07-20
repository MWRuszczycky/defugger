{-# LANGUAGE OverloadedStrings #-}

module Model.Parser
    ( parse
    , parseDebug
    , toDebug
    ) where

import qualified Data.Text   as Tx
import qualified Data.Char   as C
import qualified Data.Vector as V
import Data.List                    ( find, foldl'        )
import Data.Text                    ( Text, unpack        )
import Data.Foldable                ( asum                )
import Control.Monad.State.Lazy     ( evalStateT
                                    , get, guard
                                    , lift, put           )
import Control.Monad.Reader         ( ReaderT (..)
                                    , ask
                                    , runReaderT          )
import Control.Applicative          ( empty, many         )
import Model.Types                  ( Dictionary (..)
                                    , ErrString
                                    , BFParser
                                    , Program
                                    , DBProgram
                                    , Statement  (..)
                                    , DebugStatement (..)
                                    , Token      (..)     )

---------------------------------------------------------------------
-- Entry point

parse :: Dictionary -> Text -> Either ErrString Program
parse d = evalStateT ( runReaderT program d )

parseDebug :: Dictionary -> Text -> Either ErrString DBProgram
parseDebug d t = toDebug <$> parse d t

parseFail :: Text -> Text -> Dictionary -> BFParser a
parseFail t0 t1 d = lift . lift . Left . go . findToken tk $ d
    where t1'              = Tx.dropWhile C.isSpace t1
          (tk, _)          = splitNext d t1'
          charNo           = 1 + Tx.length t0 - Tx.length t1'
          lineNo           = "Line " ++ show ( findLineNumber charNo t0 )
          go Nothing       = lineNo ++ ": Unrecognized token: " ++ unpack tk
          go (Just BFStop) = lineNo ++ ": Unpaired close-brace for while-loop"
          go _             = lineNo ++ ": Cannot parse while-loop"

toDebug :: Program -> DBProgram
toDebug = V.fromList . (++[DBEnd]) . (DBStart:) . reverse . snd . foldl' go x0
    where x0                      = (1,[])
          go x      (DoNothing  ) = x
          go x      (Break      ) = x
          go (n,dp) (Increment  ) = (n+1, DBIncrement : dp)
          go (n,dp) (Decrement  ) = (n+1, DBDecrement : dp)
          go (n,dp) (Advance    ) = (n+1, DBAdvance   : dp)
          go (n,dp) (Backup     ) = (n+1, DBBackup    : dp)
          go (n,dp) (ReadIn     ) = (n+1, DBReadIn    : dp)
          go (n,dp) (WriteOut   ) = (n+1, DBWriteOut  : dp)
          go (n,dp) (WhileLoop p) = let (n',dq) = foldl' go (n+1,[]) p
                                        xs      = DBCloseLoop n  : dq
                                        ys      = DBOpenLoop  n' : dp
                                    in  (n'+1, xs ++ ys)

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
         BFStart -> subProgram >>= pure . WhileLoop
         BFHash  -> skipLine   >>  pure DoNothing
         _       -> pure . toStatement $ kw

opening :: BFParser Token
opening = asum . map token $ [ BFGT,    BFLT,   BFPlus
                             , BFMinus, BFDot,  BFComma
                             , BFStart, BFHash, BFBreak
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
toStatement BFBreak = Break
toStatement _       = DoNothing

splitNext :: Dictionary -> Text -> (Text, Text)
splitNext d
    | isShort d = Tx.splitAt 1
    | otherwise = Tx.break C.isSpace

findToken :: Text -> Dictionary -> Maybe Token
findToken t d = fmap fst . find ( elem t . snd ) . tokens $ d

findLineNumber :: Int -> Text -> Int
findLineNumber n = (+1) . Tx.length . Tx.filter (== '\n') . Tx.take n
