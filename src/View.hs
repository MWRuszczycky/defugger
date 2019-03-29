{-# LANGUAGE OverloadedStrings #-}

module View
    ( drawUI
    , attributes
    ) where

import qualified Data.ByteString        as BS
import qualified Graphics.Vty           as V
import qualified Brick                  as B
import qualified Brick.Widgets.Center   as B
import qualified Model.Types            as T
import qualified Data.Vector            as Vec
import Brick                                    ( (<+>), (<=>)    )
import Brick.Widgets.Border                     ( borderWithLabel )
import Model.Compiler                           ( getPosition     )
import Data.List                                ( intersperse     )

drawUI :: T.Debugger -> [ B.Widget () ]
drawUI db = [ B.vCenter . B.vBox $ ws ]
    where sep = B.padTop (B.Pad 1)
          ws  = [ programUI $ db
                , sep . titledBox "memory" . tapeUI $ db
                , sep . titledBox "output" . inputOutputUI
                      . T.output . T.computer $ db
                , sep .  titledBox "input" . inputOutputUI
                      . T.input  . T.computer $ db
                , sep . titledBox "status" . statusUI $ db
                ]

titledBox :: String -> B.Widget () -> B.Widget ()
titledBox t w = B.vBox . map B.hCenter $ [ B.str t, w ]

---------------------------------------------------------------------
-- UI for the program code

programUI :: T.Debugger -> B.Widget ()
-- ^Displays the BF code.
programUI db =
    let m  = length . show . Vec.length . T.program $ db
        ws = zipWith (formatCode db) [0..] . Vec.toList . T.program $ db
    in  borderWithLabel ( B.str "program" )
        . foldr (buildProgramUI m) B.emptyWidget
        . take (T.termHeight db) . chunksOf (T.progWidth db) $ ws

formatCode :: T.Debugger -> Int -> T.DebugStatement -> (Int, B.Widget ())
-- ^Format each BF statement or control structure for display and tag
-- with its position in the program, which is provided as an argument.
formatCode db pos x
    | pos == getPosition db  = ( pos, B.withAttr "focus" . B.str . show $ x )
    | elem pos (T.breaks db) = ( pos, B.withAttr "break" . B.str . show $ x )
    | otherwise              = ( pos, B.str . show $ x                      )

buildProgramUI :: Int -> [(Int, B.Widget ())] -> B.Widget () -> B.Widget ()
-- ^Build the program UI from each line of formatted BF statement
-- widgets tagged with their position numbers and a number width.
buildProgramUI _ []     lineWgts = lineWgts
buildProgramUI m (w:ws) lineWgts = ( nmbrWgt <+> codeWgt ) <=> lineWgts
    where nmbr    = show . fst $ w
          nmbrWgt = B.padRight (B.Pad $ m - length nmbr) . B.str $ nmbr
          codeWgt = B.hBox . snd . unzip $ w : ws

---------------------------------------------------------------------

tapeUI :: T.Debugger -> B.Widget ()
tapeUI db = B.hBox . intersperse (B.str " ") $ inBack ++ inFocus ++ inFront
    where (T.Tape xs u ys) = T.memory . T.computer $ db
          inBack           = map ( B.str . show ) . reverse $ xs
          inFocus          = [B.withAttr "focus" . B.str . show $ u]
          inFront          = map ( B.str . show ) $ ys

inputOutputUI :: BS.ByteString -> B.Widget ()
inputOutputUI bs
    | BS.null bs = B.str "<no data>"
    | otherwise  = w
    where w = B.hBox . intersperse (B.str " ")
              . map (B.str . show) . BS.unpack
              $ bs

statusUI :: T.Debugger -> B.Widget ()
statusUI db = B.hBox [ B.str $ "width = " ++ show (T.termWidth db)
                     , B.str $ " height = "  ++ show (T.termHeight db)
                     ]

attributes :: B.AttrMap
attributes = B.attrMap V.defAttr
    [ ( "focus", B.on V.black V.yellow )
    , ( "break", B.on V.red   V.black  ) ]

---------------------------------------------------------------------

chunksOf :: Int -> [a] -> [[a]]
chunksOf _ [] = []
chunksOf n xs = chnk : chunksOf n next
    where (chnk, next) = splitAt n xs
