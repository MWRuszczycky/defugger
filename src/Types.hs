module Types
    ( Tape        (..)
    , Parser      (..)
    , Statement   (..)
    , Token       (..)
    , Computer    (..)
    , ErrString   (..)
    , Program     (..)
    , BFScript    (..)
    , Computation (..)
    , Dictionary
    , BFParser    (..)
    , toDictionary
    , tokens
    , isShort
    ) where

import qualified Data.ByteString as B
import qualified Data.Text       as Tx
import Control.Monad.Reader             ( ReaderT       )
import Data.Text                        ( Text          )
import Data.Word                        ( Word8         )
import Control.Applicative              ( Alternative
                                        , empty
                                        , (<|>)         )

---------------------------------------------------------------------

data Tape a = Tape { back  :: [a]
                   , focus :: a
                   , front :: [a]
                   } deriving ( Show )

instance Functor Tape where
    fmap f (Tape xs u ys) = Tape (fmap f xs) (f u) (fmap f ys)

instance Foldable Tape where
    foldMap f (Tape xs u ys) = foldMap f (reverse xs ++ u : ys)

---------------------------------------------------------------------

-- This is the same as StateT Text Maybe a
-- Needs to be refactored

newtype Parser a = Parser { runParser :: Text -> Maybe (Text, a) }

instance Functor Parser where
    fmap f (Parser p) = Parser $ \ s -> do (s', x) <- p s
                                           Just (s', f x)

instance Applicative Parser where
    pure x                  = Parser $ \ s -> Just (s, x)
    Parser pl <*> Parser pr = Parser $ \ s -> do (s1, f) <- pl s
                                                 (s2, x) <- pr s1
                                                 Just (s2, f x)

instance Alternative Parser where
    empty                   = Parser $ \ _ -> Nothing
    Parser pl <|> Parser pr = Parser $ \ s -> pl s <|> pr s

instance Monad Parser where
    Parser p >>= g = Parser $ \ s -> do (s1, x) <- p s
                                        runParser (g x) s1

---------------------------------------------------------------------

data Computer = Computer { input  :: B.ByteString
                         , output :: B.ByteString
                         , memory :: Tape Word8
                         } deriving ( Show )

data Statement = Increment
               | Decrement
               | Advance
               | Backup
               | ReadIn
               | WriteOut
               | WhileLoop Program
               | DoNothing
                 deriving ( Show )

data Token = BFGT
           | BFLT
           | BFPlus
           | BFMinus
           | BFComma
           | BFDot
           | BFStart
           | BFStop
           | BFHash
             deriving ( Ord, Eq, Show )

data Dictionary = Dictionary { tokens  :: [ (Token, [Text]) ]
                             , isShort :: Bool
                             }

toDictionary :: [(Token, [Text])] -> Dictionary
toDictionary ts = Dictionary ts $ all ( (== 1) . Tx.length ) $ xs
    where xs = concat . snd . unzip $ ts

type Program     = [Statement]
type ErrString   = String
type Computation = Computer -> Either ErrString Computer
type BFScript    = [Token]

-- | BFParser a = ReaderT ( Dictionary -> Parser a )
type BFParser    = ReaderT Dictionary Parser
