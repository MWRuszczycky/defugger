module Model.Types
    ( BFParser
    , BFScript
    , DBProgram
    , Debugger        (..)
    , DebugStatement  (..)
    , ErrorIO
    , Computation
    , Computer        (..)
    , Dictionary      (..)
    , ErrString
    , Mode            (..)
    , Parser
    , Program
    , DefuggerOptions (..)
    , Statement       (..)
    , Status          (..)
    , Tape            (..)
    , Token           (..)
    , toDictionary
    ) where

import qualified Data.ByteString as B
import qualified Data.Text       as Tx
import qualified Data.Vector     as V
import Control.Monad.Except             ( ExceptT       )
import Control.Monad.State.Lazy         ( StateT        )
import Control.Monad.Reader             ( ReaderT       )
import Data.Text                        ( Text          )
import Data.Word                        ( Word8         )

---------------------------------------------------------------------

data DefuggerOptions = DefuggerOptions {
      mode     :: Mode
    , args     :: [String]
    , terminal :: String
}

data Mode =
      Interpreter
    | DebugMode
    | OptsError ErrString
      deriving ( Eq, Show )

---------------------------------------------------------------------

data Debugger = Debugger {
      computer   :: {-# UNPACK #-} !Computer
    , dictionary :: {-# UNPACK #-} !Dictionary
    , program    :: {-# UNPACK #-} !DBProgram
    , status     :: {-# UNPACK #-} !Status
    , readBackup :: ![Word8]
    , history    :: ![Int]
    , termWidth  :: Int
    , termHeight :: Int
    }

data Status =
      Normal
      deriving ( Eq, Show )

---------------------------------------------------------------------

data Tape a = Tape {
      back  :: ![a]
    , focus :: !a
    , front :: ![a]
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
      input  :: !B.ByteString
    , output :: !B.ByteString
    , memory :: !(Tape Word8)
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

data DebugStatement =
      DBStart
    | DBEnd
    | DBIncrement
    | DBDecrement
    | DBAdvance
    | DBBackup
    | DBReadIn
    | DBWriteOut
    | DBOpenLoop  Int
    | DBCloseLoop Int

instance Show DebugStatement where
    show DBStart         = "0"
    show DBEnd           = "0"
    show DBIncrement     = "+"
    show DBDecrement     = "-"
    show DBAdvance       = ">"
    show DBBackup        = "<"
    show DBReadIn        = ","
    show DBWriteOut      = "."
    show (DBOpenLoop  _) = "["
    show (DBCloseLoop _) = "]"

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

toDictionary :: [(Token, [Text])] -> Dictionary
toDictionary ts = Dictionary ts $ all ( (== 1) . Tx.length ) $ xs
    where xs = concat . snd . unzip $ ts

type Program     = [Statement]
type DBProgram   = V.Vector DebugStatement
type ErrString   = String
type Computation = Computer -> Either ErrString Computer
type BFScript    = [Token]

-- | Parser a   = StateT ( Text -> Maybe (a, Text) )
type Parser   = StateT Text (Either String)

-- | BFParser a = ReaderT ( Dictionary -> Parser a )
type BFParser = ReaderT Dictionary Parser

type ErrorIO = ExceptT ErrString IO
