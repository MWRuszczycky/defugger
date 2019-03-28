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
import Model.Compiler                           ( getPosition     )
import Data.List                                ( intersperse     )

drawUI :: T.Debugger -> [ B.Widget () ]
drawUI db = [ B.vCenter . B.vBox $ ws ]
    where sep = B.padTop (B.Pad 1)
          ws  = [ titledBox "program" . programUI $ db
                , sep . titledBox "memory" . tapeUI $ db
                , sep . titledBox "output" . inputOutputUI
                      . T.output . T.computer $ db
                , sep .  titledBox "input" . inputOutputUI
                      . T.input  . T.computer $ db
                , sep . titledBox "status" . statusUI $ db
                ]

titledBox :: String -> B.Widget () -> B.Widget ()
titledBox t w = B.vBox . map B.hCenter $ [ B.str t, w ]

programUI :: T.Debugger -> B.Widget ()
programUI db = B.hBox . zipWith go [0..] . Vec.toList . T.program $ db
    where go n | n == getPosition db = B.withAttr "focus" . B.str . show
               | otherwise           = B.str . show

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
    [ ( "focus", B.on V.black V.yellow ) ]
