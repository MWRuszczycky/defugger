{-# LANGUAGE OverloadedStrings #-}

module View
    ( drawUI
    , attributes
    ) where

import qualified Data.Foldable          as F
import qualified Data.ByteString        as BS
import qualified Graphics.Vty           as V
import qualified Brick                  as B
import qualified Model.Types            as T
import qualified Data.Vector            as Vec
import qualified Data.Set               as Set
import Data.List                                ( intercalate     )
import Brick.Widgets.Edit                       ( renderEditor    )
import Model.Debugger                           ( getPosition     )
import Data.Word                                ( Word8           )
import Brick                                    ( (<+>), (<=>)    )
import Brick.Widgets.Border                     ( borderWithLabel )
import Numeric                                  ( showHex         )

drawUI :: T.Debugger -> [ B.Widget T.WgtName ]
drawUI db = case T.mode db of
                 T.NormalMode -> drawNormalUI db
                 T.CommandMode -> drawCommandUI db

drawNormalUI :: T.Debugger -> [ B.Widget T.WgtName ]
drawNormalUI db = [ mainWidgets db <=> statusUI db ]

drawCommandUI :: T.Debugger -> [ B.Widget T.WgtName ]
drawCommandUI db = [ mainWidgets db <=> commandUI db ]

mainWidgets :: T.Debugger -> B.Widget T.WgtName
mainWidgets db = programUI db
                 <+> memoryUI db
                 <+> B.vBox [ outputUI db , inputUI db ]

---------------------------------------------------------------------
-- UI for the program code

programUI :: T.Debugger -> B.Widget T.WgtName
-- ^Displays the BF code.
programUI db = let m = length . show . Vec.length . T.program $ db
               in  borderWithLabel ( renderTitle T.ProgramWgt db )
                   . B.padBottom B.Max
                   . foldr ( addNumberedRow m ) B.emptyWidget
                   . slice ( T.progView db )
                   . zip [0, T.progWidth db .. ]
                   . map B.hBox
                   . chunksOf ( T.progWidth db )
                   . zipWith ( formatCode db ) [0..]
                   . Vec.toList . T.program $ db

formatCode :: T.Debugger -> Int -> T.DebugStatement -> B.Widget T.WgtName
-- ^Format each BF statement or control structure for display
formatCode db pos x
    | pos == getPosition db        = B.withAttr "focus"  . B.str . show $ x
    | pos == T.cursor db           = B.withAttr "cursor" . B.str . show $ x
    | Set.member pos (T.breaks db) = B.withAttr "break"  . B.str . show $ x
    | otherwise                    = B.str . show $ x

---------------------------------------------------------------------
-- UI for memory tape

memoryUI :: T.Debugger -> B.Widget T.WgtName
memoryUI db = let m = length . show . F.length . T.memory . T.computer $ db
              in  borderWithLabel ( renderTitle T.MemoryWgt db )
                  . B.padBottom B.Max
                  . B.hLimit 8 . B.padRight B.Max
                  . foldr ( addNumberedRow m ) B.emptyWidget
                  . slice (T.memView db)
                  . zip [0..]
                  . formatMemory $ db

formatMemory :: T.Debugger -> [B.Widget T.WgtName]
formatMemory db = inBack ++ [inFocus] ++ inFront
       where (T.Tape xs u ys) = T.memory . T.computer $ db
             inBack           = map ( B.str . show ) . reverse $ xs
             inFocus          = B.withAttr "focus" . B.str . show $ u
             inFront          = map ( B.str . show ) $ ys

---------------------------------------------------------------------
-- Input and output UIs

inputUI :: T.Debugger -> B.Widget T.WgtName
inputUI db = borderWithLabel (renderTitle T.InputWgt db)
             . B.viewport T.InputWgt B.Both
             . dataUI ( T.inFormat db )
             . T.input . T.computer $ db

outputUI :: T.Debugger -> B.Widget T.WgtName
outputUI db = borderWithLabel (renderTitle T.OutputWgt db)
              . B.viewport T.OutputWgt B.Both
              . dataUI ( T.outFormat db )
              . T.output . T.computer $ db

dataUI :: T.DataFormat -> BS.ByteString -> B.Widget T.WgtName
dataUI fmt bs
    | BS.null bs = B.str $ "<no data>"
    | otherwise  = w
    where w = case fmt of
                   T.Dec -> wrapWith toDec bs
                   T.Hex -> wrapWith toHex bs
                   T.Asc -> B.vBox . map B.str . lines
                            . concatMap toAscii
                            . BS.unpack $ bs

wrapWith :: (Word8 -> String) -> BS.ByteString -> B.Widget T.WgtName
wrapWith f bs = let m = length . show . (16*) $ quot (BS.length bs) 16
                in  B.vBox
                    . map B.str
                    . zipWith ( formatLine m f ) [0,16..]
                    . chunksOf 16
                    . BS.unpack $ bs

formatLine :: Int -> (Word8 -> String) -> Int -> [Word8] -> String
formatLine m f n xs = rightPadStr m (show n) ++ ' ' : ys
    where ys = intercalate " " . map f $ xs

rightPadStr :: Int -> String -> String
rightPadStr n s = s ++ replicate ( n - length s ) ' '

---------------------------------------------------------------------
-- Status and commandline UI

statusUI :: T.Debugger -> B.Widget T.WgtName
statusUI db
    | null msg  = B.str " "
    | otherwise = B.str msg
    where msg = T.message db

commandUI :: T.Debugger -> B.Widget T.WgtName
commandUI db = B.str ":"
               <+> ( renderEditor (B.str . unlines) True . T.commandEdit $ db )

---------------------------------------------------------------------
-- Attribute map

attributes :: B.AttrMap
attributes = B.attrMap V.defAttr
    [ ( "focus",  B.on V.black V.yellow )
    , ( "active", B.on V.green V.black )
    , ( "cursor", B.on V.black V.green  )
    , ( "lineno", B.fg V.green          )
    , ( "break",  B.fg V.red            ) ]

---------------------------------------------------------------------
-- Helpers

renderTitle :: T.WgtName -> T.Debugger -> B.Widget T.WgtName
renderTitle wn db
    | wn == T.wgtFocus db = B.withAttr "active" . B.str . show $ wn
    | otherwise           = B.str .show $ wn

--padRightBottom :: B.Padding -> B.Widget T.WgtName -> B.Widget T.WgtName
--padRightBottom p = B.padRight p . B.padBottom p

addNumberedRow :: Int -> (Int, B.Widget T.WgtName)
                  -> B.Widget T.WgtName -> B.Widget T.WgtName
-- ^Given a label width, numbered widget and accumulated widget of
-- rows of widgets, tag the numbered widget with its number and add
-- it as new row to the accumulated widget.
addNumberedRow m (n,w) rows = ( nmbrWgt <+> spacer <+> w ) <=> rows
    where spacer  = B.str " "
          nmbr    = show n
          nmbrWgt = B.withAttr "lineno"
                    . B.padRight (B.Pad $ m - length nmbr)
                    . B.str $ nmbr

chunksOf :: Int -> [a] -> [[a]]
chunksOf _ [] = []
chunksOf n xs = chnk : chunksOf n next
    where (chnk, next) = splitAt n xs

slice :: (Int, Int) -> [a] -> [a]
slice (n0, n1) = take (n1 - n0 + 1) . drop n0

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
