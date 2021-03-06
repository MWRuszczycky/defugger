{-# LANGUAGE BangPatterns      #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GADTs             #-}

module Model.Types
    ( -- Error and IO types
      ErrorIO
    , ErrString
      -- Startup data structures
    , DefuggerOptions (..)
    , RunMode         (..)
      -- Debugger model
    , DataFormat      (..)
    , DebugEvent      (..)
    , Debugger        (..)
    , Mode            (..)
    , WgtName         (..)
    , VertViewRange
      -- Debugger commands
    , HelpInfo        (..)
    , HasHelp         (..)
    , HelpExists      (..)
    , CommandBinding  (..)
    , CommandAction
    , DebuggerCommand (..)
    , Setting         (..)
    , SettingAction
    , KeyAction
    , KeyBinding      (..)
      -- Computer model
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
import qualified Graphics.Vty    as Vty
import Control.Concurrent.Async         ( Async         )
import Data.Default                     ( Default (..)  )
import Data.Set                         ( Set           )
import Data.Sequence                    ( Seq           )
import Brick.Widgets.Edit               ( Editor        )
import Brick.BChan                      ( BChan         )
import Brick.AttrMap                    ( AttrName      )
import Control.Monad.Except             ( ExceptT       )
import Control.Monad.State.Lazy         ( StateT        )
import Control.Monad.Reader             ( ReaderT       )
import Data.Text                        ( Text          )
import Data.Word                        ( Word8         )

-- =============================================================== --
-- Error and IO types

type ErrString = String
type ErrorIO   = ExceptT ErrString IO

-- =============================================================== --
-- Startup data structures used to determine how a new instance of
-- the program should run

-- |Startup state created based on command line options.
data DefuggerOptions = DefuggerOptions {
      runMode      :: RunMode        -- What mode the program is to run in
    , pathToScript :: Maybe FilePath -- File path to BF program script
    , pathToInput  :: Maybe FilePath -- File path to BF program input
    , terminal     :: String         -- Terminal settings used to update env
    , savePath     :: Maybe FilePath -- Path to save state to after a run.
    , runFast      :: Bool           -- Use the faster interpreter.
    }

instance Default DefuggerOptions where
    def = DefuggerOptions { runMode      = RunDebugger
                          , pathToScript = Nothing
                          , pathToInput  = Nothing
                          , terminal     = "xterm-256color"
                          , savePath     = Nothing
                          , runFast      = False
                          }

-- |What the program is to do based on command line arguments
data RunMode
    = RunInterpreter        -- Read and interpret a BF script
    | RunDebugger           -- Run the debugger on a BF script
    | RunHelp               -- Display startup help information
      deriving ( Eq, Show )

-- =============================================================== --
-- Modeling the debugger

-- |Model of the debugger state
data Debugger = Debugger {
      -- Core model
      computer     :: {-# UNPACK #-} !Computer    -- The computer
    , dictionary   :: !Dictionary                 -- The BF dictionary
    , program      :: !DBProgram                  -- The BF program
    , initialInput :: !B.ByteString               -- Initial input for resets
      -- Interacting with the Brick runtime system
    , channel      :: BChan DebugEvent            -- Event queue
      -- Positioning, running mode and history
    , mode         :: !Mode                       -- Current debug mode
    , wgtFocus     :: !WgtName                    -- Current focused widget
    , cursor       :: !Int                        -- Cursor position in program
    , history      :: !(Seq Int)                  -- History of statements
    , readBackup   :: ![Word8]                    -- History of reads
      -- Command and status widgets
    , commandEdit  :: Editor Text WgtName         -- Used to enter commands
    , message      :: !String                     -- Status message
      -- Terminal and display parameters
    , termWidth    :: !Int                        -- Width of the terminal
    , termHeight   :: !Int                        -- Height of the terminal
    , progView     :: !VertViewRange              -- BF script rows to render
    , memView      :: !VertViewRange              -- Memory rows to render
      -- Settings
    , breaks       :: !(Set Int)                  -- Break points
    , histDepth    :: !Int                        -- Reversion history depth
    , progWidth    :: !Int                        -- Characters shown per line
    , inFormat     :: !DataFormat                 -- Display format of input
    , outFormat    :: !DataFormat                 -- Display format of output
    , memFormat    :: !DataFormat                 -- Display format of memory
    , unsafeEdit   :: !Bool                       -- Allow unsafe edits
    , scriptPath   :: !(Maybe FilePath)           -- Path to the script
    , inputPath    :: !(Maybe FilePath)           -- Path to the input file
    }

-- |Debugger operating modes
data Mode
    = NormalMode                      -- Normal operation
    | CommandMode                     -- User entering commands
    | HelpMode Text                   -- Display help for the given commands
    | ProcessingMode (Async Debugger) -- Computiton running in separate thread

-- |Debugger custom events
data DebugEvent = ComputationDone

-- |Formats for the display of byte-values
data DataFormat
    = Asc                   -- Ascii
    | Dec                   -- Decimal
    | Hex                   -- Hexidecimal
      deriving ( Eq, Show )

-- |Names for the Brick widgets
data WgtName
    = ProgramWgt            -- Widget that displays the program
    | MemoryWgt             -- Widget that displays the memory
    | OutputWgt             -- Widget that displays current output
    | InputWgt              -- Widget that displays the input left
    | StatusWgt             -- Widget that dislays status messages
    | CommandWgt            -- Editor widget for entering commands
    | HelpWgt               -- Widget that displays help information
      deriving ( Eq, Ord )

instance Show WgtName where
    show ProgramWgt = "Program"
    show MemoryWgt  = "Memory"
    show OutputWgt  = "Output"
    show InputWgt   = "Input"
    show StatusWgt  = "Status"
    show CommandWgt = "Command Line"
    show HelpWgt    = "Help"

-- |Vertical range of lines of a wiget that are in view for display.
type VertViewRange = (Int, Int)

-- =============================================================== --
-- Help management

-- |General data type for storing help information for key-bindings
-- and command bindings.
data HelpInfo = HelpInfo {
      names     :: [ Text ] -- Ways to envoke the command
    , usage     :: Text     -- How the command is to be used
    , shortHelp :: Text     -- One-line summary information
    , longHelp  :: Text     -- Detailed help information
    } deriving ( Eq )

-- |Class of types that provide a value of HelpInfo
class HasHelp a where
    getHelp   :: a -> HelpInfo  -- The associated help information
    helpStyle :: a -> AttrName  -- Styling information for display
    helpFor   :: a -> Text      -- What is this help for

-- |This allows us to group different things that have help together.
-- This is used in the View.Help module for searching for help..
data HelpExists where
    HelpExists :: HasHelp a => a -> HelpExists

instance HasHelp HelpExists where
    getHelp (HelpExists x)   = getHelp x
    helpStyle (HelpExists x) = helpStyle x
    helpFor (HelpExists x)   = helpFor x

-- =============================================================== --
-- Debugger commands

-- |Commands that can be executed while running the debugger. Pure
-- commands have no side effects. Simple IO commands involve IO
-- actions; however, they can be run serially with the Brick runtime
-- system. Complex IO commands require that the Brick runtime system
-- be suspended while they are executed. Tandem commands are the same
-- same as pure commands; however, they should be run isolated in
-- their own thread. HScrollCmd and VScrollCmd commonds denote
-- generalized horizontal and vertical scrolling commands.
-- Seet the Controller.Commands, Controller.Settings and
-- Controller.KeyBindings for mappings between events and commands.
data DebuggerCommand
    = PureCmd      ( Debugger -> Debugger         )
    | TandemCmd    ( Debugger -> Debugger         )
    | SimpleIOCmd  ( Debugger -> ErrorIO Debugger )
    | ComplexIOCmd ( Debugger -> ErrorIO Debugger )
    | HScrollCmd WgtName Int
    | VScrollCmd WgtName Int
    | QuitCmd
    | ErrorCmd ErrString

instance Show DebuggerCommand where
    show ( PureCmd _      ) = "PureCmd"
    show ( TandemCmd _    ) = "TandemCmd"
    show ( SimpleIOCmd _  ) = "SimpleIOCmd"
    show ( ComplexIOCmd _ ) = "ComplexIOCmd"
    show ( HScrollCmd w n ) = "HScrollCmd" ++ show w ++ show n
    show ( VScrollCmd w n ) = "VScrollCmd" ++ show w ++ show n
    show ( QuitCmd        ) = "QuitCmd"
    show ( ErrorCmd e     ) = "ErrorCmd" ++ e

---------------------------------------------------------------------
-- Key bindings

-- |A KeyBinding associates a key event with a DebuggerCommand that
-- can be executed. KeyBindings are managed in Controller.KeyBindings.
data KeyBinding = KeyBinding {
      keyName   :: Vty.Key      -- The key the command is bound to
    , keyAction :: KeyAction    -- What the bound command does
    , keyHelp   :: HelpInfo     -- Help for the binding and command
    }

type KeyAction = Mode -> WgtName -> DebuggerCommand

instance HasHelp KeyBinding where
    getHelp     = keyHelp
    helpStyle _ = "keybinding"
    helpFor _   = "key-binding"

---------------------------------------------------------------------
-- Command bindings (for Command Mode)

-- |A CommandBinding associates a command that can be entered in
-- Command Mode with a DebuggerCommand that can be executed.
-- CommandBindings are managed in the Controller.CommandBindings
-- module.
data CommandBinding = CommandBinding {
      cmdNames  :: [Text]        -- The names the command is bound to
    , cmdAction :: CommandAction -- What the bound command does
    , cmdHelp   :: HelpInfo      -- Help for the bindisg and command
    }

type CommandAction = [Text] -> DebuggerCommand

instance HasHelp CommandBinding where
    getHelp     = cmdHelp
    helpStyle _ = "command"
    helpFor _   = "entered command"

---------------------------------------------------------------------
-- Settings (for use with <set>/<unset> commands)

-- |Subcommands used with the <set> and <unset> commands to modify
-- the debugger. Settings only map to pure functions on the debugger.
data Setting = Setting {
      settingName :: Text           -- Name the setting is bound to
    , setting     :: SettingAction  -- What it does under <set>
    , unsetting   :: SettingAction  -- What it does under <unset>
    , settingHelp :: HelpInfo       -- Help for the setting
    }

type SettingAction = [Text] -> Either ErrString ( Debugger -> Debugger )

instance HasHelp Setting where
    getHelp     = settingHelp
    helpStyle _ = "setting"
    helpFor _   = "debugger setting"

-- =============================================================== --
-- Model of a computer for running a BF program/script and the
-- associated computations

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

-- =============================================================== --
-- Model of BF scripts and programs

-- |A Program is a list of statements used to determine Computations.
type Program = [Statement]

-- |A DBProgram is a vector of debug statements used to determine
-- computations in the stateful context of a debugger.
type DBProgram = V.Vector DebugStatement

-- |Minimal definition of BF statements for interpretting a BF script
-- without debugging.
data Statement
    = Increment
    | Decrement
    | Advance
    | Backup
    | ReadIn
    | WriteOut
    | WhileLoop Program
    | Break
    | DoNothing
      deriving ( Eq )

instance Show Statement where
    show Increment     = "+"
    show Decrement     = "-"
    show Advance       = ">"
    show Backup        = "<"
    show ReadIn        = ","
    show WriteOut      = "."
    show (WhileLoop p) = "[" ++ concatMap show p ++ "]"
    show DoNothing     = "#\\n"
    show Break         = "B"

-- |Definition of a BF script with additional information to allow
-- for debugging operations.
data DebugStatement
    = DBStart
    | DBEnd
    | DBIncrement
    | DBDecrement
    | DBAdvance
    | DBBackup
    | DBReadIn
    | DBWriteOut
    | DBOpenLoop  Int
    | DBCloseLoop Int
      deriving ( Eq )

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

-- =============================================================== --
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
data Token
    = BFPlus        -- Increment focus of memory tape by 1
    | BFMinus       -- Decrement focus of memory tape by 1
    | BFGT          -- Advance focus of memory tape forward one unit
    | BFLT          -- Backup focus of memory tape one unit
    | BFComma       -- Read one byte from input to current focus
    | BFDot         -- Print current focus byte value to output
    | BFStart       -- Beginning of the BF program/script
    | BFStop        -- End of the BF program/script
    | BFHash        -- Denotes a comment in the program/script
    | BFBreak       -- Denotes a break point in the program/script
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

instance Default Dictionary where
    def = toDictionary [ ( BFGT,    [">"] )
                       , ( BFLT,    ["<"] )
                       , ( BFPlus,  ["+"] )
                       , ( BFMinus, ["-"] )
                       , ( BFDot,   ["."] )
                       , ( BFComma, [","] )
                       , ( BFStart, ["["] )
                       , ( BFStop,  ["]"] )
                       , ( BFHash,  ["#"] )
                       , ( BFBreak, ["B"] )
                       ]
