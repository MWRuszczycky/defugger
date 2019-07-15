{-# LANGUAGE OverloadedStrings #-}

module Model.Debugger.State
    ( -- Manipulating file paths
      toDebugPath
      -- Rendering debugger state to Text
    , programToText
      -- Rendering debugger state to bytestrings
    , debuggerToByteString
      -- Parsing debugger state
    ) where

-- =============================================================== --
-- Functions for parsing and rendering debugger state.             --
-- =============================================================== --

import qualified Model.Types             as T
import qualified Data.Foldable           as Fld
import qualified Data.ByteString         as BS
import qualified Data.ByteString.Lazy    as BSL
import qualified Data.Binary.Put         as Bin
import qualified Data.Text               as Tx
import Data.Text                                ( Text          )
import Model.Utilities                          ( chunksOf      )
import Model.Debugger.Query                     ( getPosition
                                                , getAddress    )

-- =============================================================== --
-- Manipulating file paths

toDebugPath :: FilePath -> FilePath
toDebugPath fp = case reverse . dropWhile (/= '.') . reverse $ fp of
                      [] -> fp ++ ".defug"
                      xs -> xs ++ "defug"

-- =============================================================== --
-- Rending debugger state to text

programToText :: Int -> T.Debugger -> Text
-- ^The Int argument specifies where to add line breaks.
programToText n = Tx.unlines . map Tx.pack . chunksOf n
                  . init . tail . concatMap show . Fld.toList
                  . T.program

-- =============================================================== --
-- Rendering debugger state to bytestrings

debuggerToByteString :: T.Debugger -> BS.ByteString
debuggerToByteString db = BS.concat . BSL.toChunks . Bin.runPut $ do
    -- Serialize the script length, position and script
    Bin.putWord32be . fromIntegral . Fld.length . T.program $ db
    Bin.putWord32be . fromIntegral . getPosition $ db
    putScript db
    -- Serialize the memory length, current address and values
    Bin.putWord32be . fromIntegral . Fld.length . T.memory . T.computer $ db
    Bin.putWord32be . fromIntegral . getAddress $ db
    putMemory db
    -- Serialize the output length and values
    let o = T.output . T.computer $ db
    Bin.putWord32be . fromIntegral . BS.length $ o
    Bin.putByteString o
    -- Serialize the input length and values
    let i = T.input  . T.computer $ db
    Bin.putWord32be . fromIntegral . BS.length $ i
    Bin.putByteString i

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
-- Coming soon...
