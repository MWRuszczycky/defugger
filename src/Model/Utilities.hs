module Model.Utilities
    ( -- List operations
      chunksOf
    , slice
      -- Vector operations
    , vecDelete
    , vecInsert
      -- Rendering utilities
    , toAscii
    , toAsciiAll
    , toHex
    , toDec
    ) where

-- =============================================================== --
-- Pure model utitilies                                            --
-- =============================================================== --

import qualified Data.Vector as Vec
import Data.Word ( Word8     )
import Numeric   ( showHex   )

-- =============================================================== --
-- List operations

chunksOf :: Int -> [a] -> [[a]]
chunksOf _ [] = []
chunksOf n xs = chunk : chunksOf n rest
    where (chunk, rest) = splitAt n xs

slice :: (Int, Int) -> [a] -> [a]
slice (n0, n1) = take (n1 - n0 + 1) . drop n0

-- =============================================================== --
-- Vector operations

vecDelete :: Int -> Vec.Vector a -> Vec.Vector a
vecDelete i v
    | i < 0       = v
    | Vec.null v1 = v
    | otherwise   = Vec.concat [ v0,  Vec.tail v1 ]
    where (v0,v1) = Vec.splitAt i v

vecInsert :: Int -> a -> Vec.Vector a -> Vec.Vector a
vecInsert i x v = Vec.concat [ v0, Vec.singleton x, v1 ]
    where (v0,v1) = Vec.splitAt i v

-- =============================================================== --
-- Rendering utilities

toAscii :: Word8 -> String
toAscii w
    | w == 10   = "\n"
    | w < 32    = ""
    | otherwise = [ toEnum . fromIntegral $ w ]

toAsciiAll :: Word8 -> String
toAsciiAll w
    | w < 32    = '?' : toHex w
    | otherwise = [ toEnum . fromIntegral $ w ]

toHex :: Word8 -> String
toHex w = replicate (2 - length s) '0' ++ s
    where s = showHex w ""

toDec :: Word8 -> String
toDec w = replicate (3 - length s) '0' ++ s
    where s = show w
