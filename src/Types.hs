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
    , Dictionary  (..)
    ) where

import qualified Data.ByteString as B
import qualified Data.Map        as Map
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

newtype Parser a = Parser {
    runParser :: Dictionary -> Text -> Maybe (Dictionary, Text, a) }

instance Functor Parser where
    fmap f (Parser p) = Parser $ \ d s -> do (_, s', x) <- p d s
                                             Just (d, s', f x)

instance Applicative Parser where
    pure x                  = Parser $ \ d s -> Just (d, s, x)
    Parser pl <*> Parser pr = Parser $ \ d s -> do (_, s1, f) <- pl d s
                                                   (_, s2, x) <- pr d s1
                                                   Just (d, s2, f x)

instance Alternative Parser where
    empty                   = Parser $ \ _ _ -> Nothing
    Parser pl <|> Parser pr = Parser $ \ d s -> pl d s <|> pr d s

instance Monad Parser where
    Parser p >>= g = Parser $ \ d s -> do (d, s1, x) <- p d s
                                          runParser (g x) d s1

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

type Program     = [Statement]
type ErrString   = String
type Computation = Computer -> Either ErrString Computer
type BFScript    = [Token]
type Dictionary  = Map.Map Token [Text]
