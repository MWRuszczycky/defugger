{-# LANGUAGE OverloadedStrings #-}

module View
    ( drawUI
    , attributes
    ) where

import qualified Data.Foldable          as F
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
drawUI db = [ B.vBox ws ]
    where sep = B.padTop (B.Pad 1)
          ws  = [ programUI db <+> memoryUI db
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
programUI db = let m = length . show . Vec.length . T.program $ db
               in  borderWithLabel ( B.str "program" )
                   . B.padBottom B.Max
                   . foldr    ( addNumberedRow m ) B.emptyWidget
                   . zip      [0, T.progWidth db .. ]
                   . map B.hBox
                   . take     ( T.termHeight db  )
                   . chunksOf ( T.progWidth db   )
                   . zipWith  ( formatCode db    ) [0..]
                   . Vec.toList . T.program $ db

formatCode :: T.Debugger -> Int -> T.DebugStatement -> B.Widget ()
-- ^Format each BF statement or control structure for display
formatCode db pos x
    | pos == getPosition db  = B.withAttr "focus" . B.str . show $ x
    | elem pos (T.breaks db) = B.withAttr "break" . B.str . show $ x
    | otherwise              = B.str . show $ x

---------------------------------------------------------------------
-- UI for memory tape

memoryUI :: T.Debugger -> B.Widget ()
memoryUI db = let m = length . show . F.length . T.memory . T.computer $ db
              in  borderWithLabel ( B.str "memory" )
                  . B.padBottom B.Max
                  . B.hLimit 8 . B.padRight B.Max
                  . foldr ( addNumberedRow m ) B.emptyWidget
                  . zip [0..]
                  . formatMemory $ db

formatMemory :: T.Debugger -> [B.Widget ()]
formatMemory db = inBack ++ [inFocus] ++ inFront
       where (T.Tape xs u ys) = T.memory . T.computer $ db
             inBack           = map ( B.str . show ) . reverse $ xs
             inFocus          = B.withAttr "focus" . B.str . show $ u
             inFront          = map ( B.str . show ) $ ys

---------------------------------------------------------------------
-- UI for memory tape

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
-- Helpers

addNumberedRow :: Int -> (Int, B.Widget ()) -> B.Widget () -> B.Widget ()
-- ^Given a label width, numbered widget and accumulated widget of
-- rows of widgets, tag the numbered widget with its number and add
-- it as new row to the accumulated widget.
addNumberedRow m (n,w) rows = ( nmbrWgt <+> spacer <+> w ) <=> rows
    where nmbrWgt = B.padRight (B.Pad $ m - length nmbr) . B.str $ nmbr
          spacer  = B.str " "
          nmbr    = show n

chunksOf :: Int -> [a] -> [[a]]
chunksOf _ [] = []
chunksOf n xs = chnk : chunksOf n next
    where (chnk, next) = splitAt n xs
