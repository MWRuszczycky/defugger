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
import Brick                        ( (<=>), (<+>)      )
import Brick.Widgets.Border         ( borderAttr        )
import Brick.Widgets.Edit           ( editAttr
                                    , editFocusedAttr   )

-- =============================================================== --
-- Managing attributes

attributes :: B.AttrMap
-- ^The default attribute map.
attributes = B.attrMap V.defAttr
    [ ( "background",    B.bg           V.black  )
    , ( "focus",         B.on V.black   V.yellow )
    , ( "active",        B.on V.green   V.black  )
    , ( "cursor",        B.on V.black   V.green  )
    , ( "lineno",        B.on V.green   V.black  )
    , ( "break",         B.on V.black   V.red    )
    , ( "highlight",     B.on V.magenta V.black  )
    , ( "command",       B.on V.green   V.black  )
    , ( "header",        B.on V.blue    V.black  )
    , ( "setting",       B.on V.blue    V.black  )
    , ( "keybinding",    B.on V.yellow  V.black  )
    , ( borderAttr,      B.on V.white   V.black  )
    , ( editAttr,        B.on V.white   V.black  )
    , ( editFocusedAttr, B.on V.white   V.black  )
    ]

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
