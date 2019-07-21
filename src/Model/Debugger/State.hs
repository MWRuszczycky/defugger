{-# LANGUAGE OverloadedStrings #-}

module Model.Debugger.State
    ( -- Rendering debugger state to Text
      programToText
      -- Rendering debugger state to bytestrings
    , encodeDebugger
    , encodeResult
      -- Parsing debugger state
    , decodeDebugger
    ) where

-- =============================================================== --
-- Functions for parsing and rendering debugger state.             --
-- =============================================================== --

import qualified Model.Types             as T
import qualified Data.Foldable           as Fld
import qualified Data.ByteString         as BS
import qualified Data.ByteString.Lazy    as BSL
import qualified Data.Binary.Put         as Bin
import qualified Data.Binary.Get         as Bin
import qualified Data.Text               as Tx
import qualified Data.Vector             as Vec
import qualified Data.Set                as Set
import qualified Data.Sequence           as Seq
import Data.Sequence                            ( (<|)          )
import Data.Bifunctor                           ( bimap         )
import Control.Monad                            ( replicateM
                                                , guard         )
import Data.Word                                ( Word8         )
import Data.Text                                ( Text          )
import Model.Utilities                          ( chunksOf      )
import Model.Debugger.Query                     ( getPosition   )

-- =============================================================== --
-- Rending debugger state to text

programToText :: Int -> T.Debugger -> Text
-- ^The Int argument specifies where to add line breaks.
programToText n = Tx.unlines . map Tx.pack . chunksOf n
                  . init . tail . concatMap show . Fld.toList
                  . T.program

-- =============================================================== --
-- Rendering debugger state to bytestrings

encodeDebugger :: T.Debugger -> BS.ByteString
encodeDebugger db = BSL.toStrict . Bin.runPut $ do
    putScript (T.program db) (getPosition db)
    putBreaks . T.breaks $ db
    putComputer . T.computer $ db
    putBytes . T.initialInput $ db
    Bin.putWord8 0x0a

encodeResult :: (T.Computer, T.DBProgram, Set.Set Int, BS.ByteString)
                -> BS.ByteString
encodeResult (c,p,bs,i) = BSL.toStrict . Bin.runPut $ do
    -- There are always at least two break points: the start and the
    -- and. The program position after a run is always the second.
    let position = Set.toList bs !! 1
    putScript p position
    putBreaks bs
    putComputer c
    putBytes i
    Bin.putWord8 0x0a

---------------------------------------------------------------------
-- Helpers for the Bin.Put monad

putIntAsWord32be :: Int -> Bin.Put
putIntAsWord32be = Bin.putWord32be . fromIntegral

putBytes :: BS.ByteString -> Bin.Put
putBytes bs = do
    putIntAsWord32be . BS.length $ bs
    Bin.putByteString bs

putComputer :: T.Computer -> Bin.Put
putComputer c =  do
    -- Memory size
    putIntAsWord32be . Fld.length . T.memory $ c
    -- Address of the head
    putIntAsWord32be . length . T.front . T.memory $ c
    -- Serialize the memory
    Fld.traverse_ Bin.putWord8 . T.memory $ c
    -- Serialize the output length and values
    putBytes . T.output $ c
    -- Serialize the remaining input length and values
    putBytes . T.input $ c

putBreaks :: Set.Set Int -> Bin.Put
putBreaks bs = do
    putIntAsWord32be . Set.size $ bs
    mapM_ putIntAsWord32be bs

putScript :: T.DBProgram -> Int -> Bin.Put
putScript p n = do
    putIntAsWord32be . Fld.length $ p
    putIntAsWord32be n
    Fld.traverse_ putStatement p

putStatement :: T.DebugStatement -> Bin.Put
putStatement (T.DBStart      ) = Bin.putCharUtf8 '0'
putStatement (T.DBEnd        ) = Bin.putCharUtf8 '0'
putStatement (T.DBIncrement  ) = Bin.putCharUtf8 '+'
putStatement (T.DBDecrement  ) = Bin.putCharUtf8 '-'
putStatement (T.DBAdvance    ) = Bin.putCharUtf8 '>'
putStatement (T.DBBackup     ) = Bin.putCharUtf8 '<'
putStatement (T.DBReadIn     ) = Bin.putCharUtf8 ','
putStatement (T.DBWriteOut   ) = Bin.putCharUtf8 '.'
putStatement (T.DBOpenLoop  n) = Bin.putCharUtf8 '[' >> putIntAsWord32be n
putStatement (T.DBCloseLoop n) = Bin.putCharUtf8 ']' >> putIntAsWord32be n

-- =============================================================== --
-- Parsing debugger state

decodeDebugger :: T.Debugger -> BS.ByteString -> Either T.ErrString T.Debugger
decodeDebugger db bs = do
    let err _ = "Unable to read input defug file."
    (_,_,r) <- bimap err id . Bin.runGetOrFail getDebugger . BSL.fromStrict $ bs
    let (c,p,x,b,ii) = r
    pure $ db { T.computer     = c
              , T.program      = p
              , T.history      = x <| Seq.Empty
              , T.cursor       = x
              , T.breaks       = b
              , T.initialInput = ii
              , T.scriptPath   = Nothing
              , T.inputPath    = Nothing
              }

getDebugger :: Bin.Get ( T.Computer      -- The decoded computer
                       , T.DBProgram     -- The decoded program
                       , Int             -- The program position
                       , Set.Set Int     -- The break points
                       , BS.ByteString ) -- The initial input
getDebugger = do
    -- Get the script length, position and script
    lenScript <- fromIntegral <$> Bin.getWord32be
    posScript <- fromIntegral <$> Bin.getWord32be
    script    <- getScript lenScript
    -- Get the break points
    lenBreaks <- fromIntegral <$> Bin.getWord32be
    breaks    <- map fromIntegral <$> replicateM lenBreaks Bin.getWord32be
    -- Get the memory length, current address and values
    lenMemory <- fromIntegral <$> Bin.getWord32be
    address   <- fromIntegral <$> Bin.getWord32be
    memory    <- getMemory lenMemory address
    -- Get the output
    lenOutput <- fromIntegral <$> Bin.getWord32be
    output    <- Bin.getByteString lenOutput
    -- Get the input
    lenInput  <- fromIntegral <$> Bin.getWord32be
    input     <- Bin.getByteString lenInput
    -- Get the initial input
    lenIInput <- fromIntegral <$> Bin.getWord32be
    iInput    <- Bin.getByteString lenIInput
    -- Read the end-of-line at the end of the file and end of input
    lastByte  <- Bin.getWord8
    isEoF     <- Bin.isEmpty
    guard $ lastByte == 0x0a && isEoF
    pure ( T.Computer input output memory
         , script
         , posScript
         , Set.fromList breaks
         , iInput
         )

getScript :: Int -> Bin.Get T.DBProgram
getScript count = Vec.fromList . (T.DBStart:) . tail <$> replicateM count go
    where go = do next <- toEnum . fromIntegral <$> Bin.getWord8
                  case next of
                       '+' -> pure T.DBIncrement
                       '-' -> pure T.DBDecrement
                       '>' -> pure T.DBAdvance
                       '<' -> pure T.DBBackup
                       '.' -> pure T.DBWriteOut
                       ',' -> pure T.DBReadIn
                       '[' -> fromIntegral <$> Bin.getWord32be
                              >>= pure . T.DBOpenLoop
                       ']' -> fromIntegral <$> Bin.getWord32be
                              >>= pure . T.DBCloseLoop
                       _   -> pure T.DBEnd

getMemory :: Int -> Int -> Bin.Get (T.Tape Word8)
getMemory len index = go <$> Bin.getByteString len
    where go bs | null ys   = T.Tape [] 0 []
                | otherwise = T.Tape (reverse xs) (head ys) (tail ys)
                where (xs,ys) = splitAt index . BS.unpack $ bs
