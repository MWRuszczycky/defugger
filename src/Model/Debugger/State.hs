{-# LANGUAGE OverloadedStrings #-}

module Model.Debugger.State
    ( -- Manipulating file paths
      toDebugPath
      -- Rendering debugger state
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
import qualified Data.ByteString.Builder as Bld
import Model.Debugger.Query                     ( getPosition
                                                , getAddress    )

-- =============================================================== --
-- Manipulating file paths

toDebugPath :: FilePath -> FilePath
toDebugPath fp = case reverse . dropWhile (/= '.') . reverse $ fp of
                      [] -> fp ++ ".defug"
                      xs -> xs ++ "defug"

-- =============================================================== --
-- Rendering debugger state as a bytestring

debuggerToByteString :: T.Debugger -> BS.ByteString
debuggerToByteString db =
    let p = T.program $ db
        m = T.memory . T.computer $ db
        o = T.output . T.computer $ db
        i = T.input  . T.computer $ db
    in  BS.concat . BSL.toChunks . Bld.toLazyByteString . mconcat $
            [ Bld.word32BE . fromIntegral . Fld.length $ p
            , Bld.word32BE . fromIntegral . getPosition $ db
            , buildScript db
            , Bld.word32BE . fromIntegral . Fld.length $ m
            , Bld.word32BE . fromIntegral . getAddress $ db
            , buildMemory db
            , Bld.word32BE . fromIntegral . BS.length $ o
            , Bld.byteString o
            , Bld.word32BE . fromIntegral . BS.length $ i
            , Bld.byteString i
            ]

buildScript :: T.Debugger -> Bld.Builder
buildScript = mconcat . map go . Fld.toList . T.program
    where to32BE                 = Bld.word32BE . fromIntegral
          go (T.DBStart        ) = Bld.char8 '0'
          go (T.DBEnd          ) = Bld.char8 '0'
          go (T.DBIncrement    ) = Bld.char8 '+'
          go (T.DBDecrement    ) = Bld.char8 '-'
          go (T.DBAdvance      ) = Bld.char8 '>'
          go (T.DBBackup       ) = Bld.char8 '<'
          go (T.DBReadIn       ) = Bld.char8 ','
          go (T.DBWriteOut     ) = Bld.char8 '.'
          go (T.DBOpenLoop  n  ) = Bld.char8 '[' <> to32BE n
          go (T.DBCloseLoop n  ) = Bld.char8 ']' <> to32BE n

buildMemory :: T.Debugger -> Bld.Builder
buildMemory = mconcat . map Bld.word8 . Fld.toList . T.memory . T.computer

-- =============================================================== --
-- Parsing debugger state
-- Coming soon...
