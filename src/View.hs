{-# LANGUAGE OverloadedStrings #-}

module View
    ( drawUI
    , attributes
    ) where

import qualified Graphics.Vty as V
import qualified Brick        as B
import qualified Model.Types  as T
import Data.List                    ( intersperse )
import Brick.Widgets.Border         ( border      )

drawUI :: T.Debugger -> [ B.Widget () ]
drawUI db = [ B.vBox [ border $ programUI db, border $ tapeUI db ] ]

programUI :: T.Debugger -> B.Widget ()
programUI db = B.hBox . zipWith go [0..] . T.program $ db
    where go n | n == T.position db = B.withAttr "focus" . B.str . show
               | otherwise          = B.str . show

tapeUI :: T.Debugger -> B.Widget ()
tapeUI db = B.hBox . intersperse (B.str " ") $ inBack ++ inFocus ++ inFront
    where (T.Tape xs u ys) = T.memory . T.computer $ db
          inBack           = map ( B.str . show ) . reverse $ xs
          inFocus          = [B.withAttr "focus" . B.str . show $ u]
          inFront          = map ( B.str . show ) $ ys

attributes :: B.AttrMap
attributes = B.attrMap V.defAttr
    [ ( "focus", B.fg V.yellow ) ]
