module Model.Types
    ( -- Error types
      ErrorIO
    , ErrString
      -- Startup data structures
    , DefuggerOptions (..)
    , Mode            (..)
      -- Debugger model
    , DataFormat      (..)
    , Debugger        (..)
    , Status          (..)
    , WgtName         (..)
      -- Computer/Computation model
    , Computation
    , Computer        (..)
    , Tape            (..)
      -- BF scripts and programs
    , DBProgram
    , DebugStatement  (..)
    , Program
    , Statement       (..)
      -- Parsing BF scripts
    , BFParser
    , BFScript
    , Dictionary      (isShort, tokens)
    , Parser
    , Token           (..)
    , toDictionary
    ) where

import qualified Data.ByteString as B
import qualified Data.Text       as Tx
import qualified Data.Vector     as V
import Control.Monad.Except             ( ExceptT   )
import Control.Monad.State.Lazy         ( StateT    )
import Control.Monad.Reader             ( ReaderT   )
import Data.Text                        ( Text      )
import Data.Word                        ( Word8     )

---------------------------------------------------------------------
-- Error types

type ErrString = String
type ErrorIO   = ExceptT ErrString IO

---------------------------------------------------------------------
-- Startup data structures used to determine how a new instance of
-- the program should run

-- |Startup state created based on command line options.
data DefuggerOptions = DefuggerOptions {
      mode     :: Mode      -- What mode the program is to run in
    , args     :: [String]  -- Additional command line arguments
    , terminal :: String    -- Terminal settings used to update env
}

-- |What the program is to do based on command line arguments
data Mode =
      Interpreter           -- Read and interpret a BF script
    | DebugMode             -- Run the debugger on a BF script
    | OptsError ErrString   -- Display an error message and quit
      deriving ( Eq, Show )

---------------------------------------------------------------------
-- Modeling the debugger

-- |Model of the debugger state
data Debugger = Debugger {
      computer   :: {-# UNPACK #-} !Computer    -- The computer
    , dictionary :: {-# UNPACK #-} !Dictionary  -- The BF dictionary
    , program    :: {-# UNPACK #-} !DBProgram   -- The BF program
    , status     :: {-# UNPACK #-} !Status      -- Current debug status
    , wgtFocus   :: !WgtName                    -- Current focused widget
    , readBackup :: ![Word8]                    -- History of reads
    , history    :: ![Int]                      -- History of statements
    , cursor     :: !Int                        -- Cursor position in program
    , breaks     :: ![Int]                      -- User specified break points
    , termWidth  :: !Int                        -- Width of the terminal
    , termHeight :: !Int                        -- Height of the terminal
    , progWidth  :: !Int                        -- BF characters shown per line
    , inFormat   :: !DataFormat                 -- Display format of input data
    , outFormat  :: !DataFormat                 -- Display format of output data
    , progView   :: !(Int,Int)                  -- BF script rows to render
    , memView    :: !(Int,Int)                  -- Memory rows to render
    }

-- |Status of the debugger
data Status =
      Normal                -- Normal operation
      deriving ( Eq, Show )

-- |Formats for the display of byte-values
data DataFormat =
      Asc                   -- Ascii
    | Dec                   -- Decimal
    | Hex                   -- Hexidecimal
      deriving ( Eq, Show )

-- |Names for the Brick widgets
data WgtName =
      ProgramWgt            -- Widget that displays the program
    | MemoryWgt             -- Widget that displays the memory
    | OutputWgt             -- Widget that displays current output
    | InputWgt              -- Widget that displays the input left
    | StatusWgt             -- Widget that dislays status messages
      deriving ( Eq, Ord, Show )

---------------------------------------------------------------------
-- Model of a computer for running a BF program/script and the
-- associated computations

-- |A Computation converts a computer between states and can fail.
type Computation = Computer -> Either ErrString Computer

-- |Memory tape of a simple computer that can run a BF program.
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

-- |Model of a computer that can run a BF program.
data Computer = Computer {
      input  :: !B.ByteString   -- Input data read using ','
    , output :: !B.ByteString   -- Output data set using '.'
    , memory :: !(Tape Word8)   -- Memory tape
    } deriving ( Show )

---------------------------------------------------------------------
-- Model of BF scripts and programs

-- |A Program is a list of statements used to determine Computations.
type Program     = [Statement]

-- |A DBProgram is a vector of debug statements used to determine
-- computations in the stateful context of a debugger.
type DBProgram   = V.Vector DebugStatement

-- |Minimal definition of BF statements for interpretting a BF script
-- without debugging.
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

-- |Definition of a BF script with additional information to allow
-- for debugging operations.
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

---------------------------------------------------------------------
-- Parsing BF scripts

-- |A BFScript is a list of standardized tokens used to parse a BF
-- script where alternative tokens can be specified via a Dictionary.
type BFScript = [Token]

-- |A Parser is a generalize parser for Text strings.
-- Parser a = StateT ( Text -> Maybe (a, Text) )
type Parser = StateT Text (Either String)

-- |A BFParser is a parser used to convert BF scripts to BF programs.
-- BFParser a = ReaderT ( Dictionary -> Parser a )
type BFParser = ReaderT Dictionary Parser

-- |A Token is a standardized command that can be specified using BF.
data Token =
      BFPlus        -- Increment focus of memory tape by 1
    | BFMinus       -- Decrement focus of memory tape by 1
    | BFGT          -- Advance focus of memory tape forward one unit
    | BFLT          -- Backup focus of memory tape one unit
    | BFComma       -- Read one byte from input to current focus
    | BFDot         -- Print current focus byte value to output
    | BFStart       -- Beginning of the BF program/script
    | BFStop        -- End of the BF program/script
    | BFHash        -- Denotes a comment in the program/script
      deriving ( Ord, Eq, Show )

-- |A Dictionary provides a specification for how a BF script can be
-- written. This allows alternative ways of writing BF script using
-- keywords or characters other than those specified by the standard.
-- A new Dictionary value should be instantiated using toDictionary.
-- Dictionaries that have only single-character tokens do not need
-- spaces to separate the tokens in the script.
data Dictionary = Dictionary {
      tokens  :: [ (Token, [Text]) ]    -- Synonym dictionary
    , isShort :: Bool                   -- All synonyms are single
    } deriving ( Show )                 -- characters

toDictionary :: [(Token, [Text])] -> Dictionary
-- ^Should be used to instantiate a new dictionary so that all fields
-- are correctly set. In other words, use toDictionary not Dictionary
-- to create a new value of type Dictionary.
toDictionary ts = Dictionary ts $ all ( (== 1) . Tx.length ) $ xs
    where xs = concat . snd . unzip $ ts
