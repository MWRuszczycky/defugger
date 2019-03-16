module Types
    ( BFParser    (..)
    , BFScript    (..)
    , Computation (..)
    , Computer    (..)
    , Dictionary  (..)
    , ErrString   (..)
    , Parser      (..)
    , Program     (..)
    , Statement   (..)
    , Tape        (..)
    , Token       (..)
    , dictionary
    ) where

import qualified Data.ByteString as B
import qualified Data.Text       as Tx
import Control.Monad.State.Lazy         ( StateT        )
import Control.Monad.Reader             ( ReaderT       )
import Data.Text                        ( Text          )
import Data.Word                        ( Word8         )
import Control.Applicative              ( Alternative
                                        , (<|>), empty  )

---------------------------------------------------------------------

data Tape a = Tape {
      back  :: [a]
    , focus ::  a
    , front :: [a]
    }

instance Show a => Show (Tape a) where
    show (Tape xs u ys) = unwords . concat $ t
        where t = [ map show . reverse $ xs
                  , ["[" ++ show u ++ "]"]
                  , map show ys
                  ]

instance Functor Tape where
    fmap f (Tape xs u ys) = Tape (fmap f xs) (f u) (fmap f ys)

instance Foldable Tape where
    foldMap f (Tape xs u ys) = foldMap f (reverse xs ++ u : ys)

---------------------------------------------------------------------

data Computer = Computer {
      input  :: B.ByteString
    , output :: B.ByteString
    , memory :: Tape Word8
    } deriving ( Show )

data Statement =
      Increment
    | Decrement
    | Advance
    | Backup
    | ReadIn
    | WriteOut
    | WhileLoop Program
    | DoNothing

instance Show Statement where
    show Increment     = "+"
    show Decrement     = "-"
    show Advance       = ">"
    show Backup        = "<"
    show ReadIn        = ","
    show WriteOut      = "."
    show (WhileLoop p) = "[" ++ concatMap show p ++ "]"
    show DoNothing     = "#\\n"

data Token =
      BFPlus
    | BFMinus
    | BFGT
    | BFLT
    | BFComma
    | BFDot
    | BFStart
    | BFStop
    | BFHash
      deriving ( Ord, Eq, Show )

data Dictionary = Dictionary {
      tokens  :: [ (Token, [Text]) ]
    , isShort :: Bool
    } deriving ( Show )

dictionary :: [(Token, [Text])] -> Dictionary
dictionary ts = Dictionary ts $ all ( (== 1) . Tx.length ) $ xs
    where xs = concat . snd . unzip $ ts

type Program     = [Statement]
type ErrString   = String
type Computation = Computer -> Either ErrString Computer
type BFScript    = [Token]

-- | Parser a   = StateT ( Text -> Maybe (a, Text) )
type Parser   = StateT Text (Either String)

-- | BFParser a = ReaderT ( Dictionary -> Parser a )
type BFParser = ReaderT Dictionary Parser
