{-# LANGUAGE OverloadedStrings #-}

module View.Core
    ( -- Managing attributes
      attributes
      -- Widget utilities
    , addNumberedRow
    , renderTitle
    ) where

-- =============================================================== --
-- Core rendering utilities for the View                           --
-- =============================================================== --

import qualified Graphics.Vty as V
import qualified Brick        as B
import qualified Model.Types  as T
import Brick                        ( (<=>), (<+>) )

-- =============================================================== --
-- Managing attributes

attributes :: B.AttrMap
-- ^The default attribute map.
attributes = B.attrMap V.defAttr
    [ ( "focus",  B.on V.black V.yellow )
    , ( "active", B.on V.green V.black )
    , ( "cursor", B.on V.black V.green  )
    , ( "lineno", B.fg V.green          )
    , ( "break",  B.fg V.red            ) ]

-- =============================================================== --
-- Widget utilities

renderTitle :: T.WgtName -> T.Debugger -> B.Widget T.WgtName
renderTitle wn db
    | wn == T.wgtFocus db = B.withAttr "active" . B.str . show $ wn
    | otherwise           = B.str .show $ wn

addNumberedRow :: Int -> (Int, B.Widget T.WgtName)
                  -> B.Widget T.WgtName -> B.Widget T.WgtName
-- ^Given a label width, numbered widget and accumulated widget of
-- rows of widgets, tag the numbered widget with its number and add
-- it as a new row to the accumulated widget.
addNumberedRow m (n,w) rows = ( nmbrWgt <+> spacer <+> w ) <=> rows
    where spacer  = B.str " "
          nmbr    = show n
          nmbrWgt = B.withAttr "lineno"
                    . B.padRight (B.Pad $ m - length nmbr)
                    . B.str $ nmbr
