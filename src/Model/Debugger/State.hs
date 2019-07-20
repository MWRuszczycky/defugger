{-# LANGUAGE OverloadedStrings #-}

module Model.Debugger.State
    ( -- Rendering debugger state to Text
      programToText
      -- Rendering debugger state to bytestrings
    , encodeDebugger
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
import Model.Debugger.Query                     ( getPosition
                                                , getAddress    )

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
    -- Serialize the script length, position and script
    Bin.putWord32be . fromIntegral . Fld.length . T.program $ db
    Bin.putWord32be . fromIntegral . getPosition $ db
    putScript db
    -- Serialize the break points
    Bin.putWord32be . fromIntegral . Set.size . T.breaks $ db
    mapM_ (Bin.putWord32be . fromIntegral) . Set.toList . T.breaks $ db
    -- Serialize the memory length, current address and values
    Bin.putWord32be . fromIntegral . Fld.length . T.memory . T.computer $ db
    Bin.putWord32be . fromIntegral . getAddress $ db
    putMemory db
    -- Serialize the output length and values
    let o = T.output . T.computer $ db
    Bin.putWord32be . fromIntegral . BS.length $ o
    Bin.putByteString o
    -- Serialize the remaining input length and values
    let i = T.input  . T.computer $ db
    Bin.putWord32be . fromIntegral . BS.length $ i
    Bin.putByteString i
    -- Serialie the initial input length and values
    let ii = T.initialInput db
    Bin.putWord32be . fromIntegral . BS.length $ ii
    Bin.putByteString ii
    -- End with a newline
    Bin.putWord8 0x0a

putScript :: T.Debugger -> Bin.Put
putScript = Fld.traverse_ go . T.program
    where toPut32be            = Bin.putWord32be . fromIntegral
          go (T.DBStart      ) = Bin.putCharUtf8 '0'
          go (T.DBEnd        ) = Bin.putCharUtf8 '0'
          go (T.DBIncrement  ) = Bin.putCharUtf8 '+'
          go (T.DBDecrement  ) = Bin.putCharUtf8 '-'
          go (T.DBAdvance    ) = Bin.putCharUtf8 '>'
          go (T.DBBackup     ) = Bin.putCharUtf8 '<'
          go (T.DBReadIn     ) = Bin.putCharUtf8 ','
          go (T.DBWriteOut   ) = Bin.putCharUtf8 '.'
          go (T.DBOpenLoop  n) = Bin.putCharUtf8 '[' >> toPut32be n
          go (T.DBCloseLoop n) = Bin.putCharUtf8 ']' >> toPut32be n

putMemory :: T.Debugger -> Bin.Put
putMemory = Fld.traverse_ Bin.putWord8 . T.memory . T.computer

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
