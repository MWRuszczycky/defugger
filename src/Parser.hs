{-# LANGUAGE OverloadedStrings #-}

module Parser
    ( parse
    ) where

import qualified Types      as T
import qualified Data.Text  as Tx
import qualified Data.Map   as Map
import qualified Data.Char  as C
import Data.List                    ( maximumBy
                                    , minimumBy
                                    , find          )
import Data.Text                    ( Text          )
import Control.Applicative          ( Alternative
                                    , (<|>)
                                    , many, empty   )

---------------------------------------------------------------------

parse :: T.Dictionary -> Text -> Maybe T.Program
parse d = maybe Nothing ( \ (_,_,x) -> Just x ) . T.runParser program d

program :: T.Parser T.Program
program = do
    p <- many statement
    endOfInput
    pure p

statement :: T.Parser T.Statement
statement = do
    separator
    kw <- opening
    case kw of
         T.BFStart -> routine >>= pure . T.WhileLoop
         T.BFHash  -> skipLine >> pure T.DoNothing
         otherwise -> pure . toStatement $ kw

endOfInput :: T.Parser ()
endOfInput = T.Parser $ \ d s ->
    if Tx.null s
       then Just (d, Tx.empty, ())
       else Nothing

opening :: T.Parser T.Token
opening = choice . map token $ axns
    where axns = [ T.BFGT,  T.BFLT,    T.BFPlus,  T.BFMinus
                 , T.BFDot, T.BFComma, T.BFStart, T.BFHash
                 ]

routine :: T.Parser T.Program
routine = do
    p <- many statement
    stop >> pure p

choice :: [T.Parser a] -> T.Parser a
choice []     = empty
choice (x:[]) = x
choice (x:xs) = x <|> choice xs

stop :: T.Parser T.Token
stop = token T.BFStop

start :: T.Parser T.Token
start = token T.BFStart

token :: T.Token -> T.Parser T.Token
token tok = T.Parser $ \ d s ->
    case findPrefix d tok s of
         Nothing -> Nothing
         Just x  -> let s' = Tx.drop (Tx.length x) s
                    in  Just ( d, s', tok )

separator :: T.Parser ()
separator = many ( satisfy C.isSpace ) >> pure ()

satisfy :: (Char -> Bool) -> T.Parser Char
satisfy f = T.Parser $ \ d s ->
    case Tx.uncons s of
         Nothing     -> Nothing
         Just (t,ts) -> if f t then Just (d, ts, t) else Nothing

skipLine :: T.Parser ()
skipLine = T.Parser $ \ d s ->
    case Tx.uncons . Tx.dropWhile (/= '\n') $ s of
         Nothing      -> Just (d, Tx.empty, ())
         Just (_, s') -> Just (d, s',       ())

toStatement :: T.Token -> T.Statement
toStatement T.BFGT    = T.Advance
toStatement T.BFLT    = T.Backup
toStatement T.BFPlus  = T.Increment
toStatement T.BFMinus = T.Decrement
toStatement T.BFComma = T.ReadIn
toStatement T.BFDot   = T.WriteOut
toStatement _         = T.DoNothing

bfDict :: T.Dictionary
bfDict =  Map.fromList [ ( T.BFGT,    [">"] )
                       , ( T.BFLT,    ["<"] )
                       , ( T.BFPlus,  ["+"] )
                       , ( T.BFMinus, ["-"] )
                       , ( T.BFDot,   ["."] )
                       , ( T.BFComma, [","] )
                       , ( T.BFStart, ["["] )
                       , ( T.BFStop,  ["]"] )
                       , ( T.BFHash,  ["#"] )
                       ]

testProg :: Text
testProg = "++> ++[<+>-]-->> # a comment \n <<[<>--++[+<->+]]->+"
---------------------------------------------------------------------

findPrefix :: T.Dictionary -> T.Token -> Text -> Maybe Text
findPrefix d tok txt = case Map.lookup tok d of
                            Nothing -> Nothing
                            Just xs -> let mn = minLength xs
                                           mx = maxLength xs
                                       in find (flip elem xs)
                                          $ [ Tx.take n txt | n <- [mn..mx] ]

compareLength :: Text -> Text -> Ordering
compareLength x y = compare ( Tx.length x ) ( Tx.length y )

maxLength :: [Text] -> Int
maxLength = Tx.length . maximumBy compareLength

minLength :: [Text] -> Int
minLength = Tx.length . minimumBy compareLength
