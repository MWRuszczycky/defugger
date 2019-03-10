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
    ) where

import qualified Data.ByteString as B
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

newtype Parser t a = Parser { runParser :: [t] -> Maybe ([t], a) }

instance Functor (Parser t) where
    fmap f (Parser p) = Parser $ \ s -> do (s', x) <- p s
                                           Just (s', f x)

instance Applicative (Parser t) where
    pure x                  = Parser $ \ s -> Just (s, x)
    Parser pl <*> Parser pr = Parser $ \ s -> do (s1, f) <- pl s
                                                 (s2, x) <- pr s1
                                                 Just (s2, f x)

instance Alternative (Parser t) where
    empty                   = Parser $ \ s -> Nothing
    Parser pl <|> Parser pr = Parser $ \ s -> pl s <|> pr s

instance Monad (Parser t) where
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
               | Skip
                 deriving ( Show )

data Token = BFGT
           | BFLT
           | BFPlus
           | BFMinus
           | BFComma
           | BFDot
           | BFStart
           | BFStop
             deriving ( Eq, Show )

type Program     = [Statement]
type ErrString   = String
type Computation = Computer -> Either ErrString Computer
type BFScript    = [Token]
