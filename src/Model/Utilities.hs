module Model.Utilities
    ( -- List operations
      chunksOf
    , slice
      -- Rendering utilities
    , toAscii
    , toHex
    , toDec
    ) where

-- =============================================================== --
-- Pure model utitilies                                            --
-- =============================================================== --

import Data.Word ( Word8     )
import Numeric   ( showHex   )

-- =============================================================== --
-- List operations

chunksOf :: Int -> [a] -> [[a]]
chunksOf _ [] = []
chunksOf n xs = chnk : chunksOf n next
    where (chnk, next) = splitAt n xs

slice :: (Int, Int) -> [a] -> [a]
slice (n0, n1) = take (n1 - n0 + 1) . drop n0

-- =============================================================== --
-- Rendering utilities

toAscii :: Word8 -> String
toAscii w
    | w == 10   = "\n"
    | w < 32    = ""
    | otherwise = [ toEnum . fromIntegral $ w ]

toHex :: Word8 -> String
toHex w = replicate (2 - length s) '0' ++ s
    where s = showHex w ""

toDec :: Word8 -> String
toDec w = replicate (3 - length s) '0' ++ s
    where s = show w
