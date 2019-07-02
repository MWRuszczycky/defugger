{-# LANGUAGE OverloadedStrings #-}

module View.View
    ( drawUI
    ) where

-- =============================================================== --
-- Rendering the debugger interface using Brick                    --
-- =============================================================== --

import qualified Data.Foldable          as F
import qualified Data.ByteString        as BS
import qualified Brick                  as B
import qualified Data.Vector            as Vec
import qualified Data.Set               as Set
import qualified Model.Types            as T
import Data.List                                ( intercalate       )
import Brick.Widgets.Edit                       ( renderEditor      )
import Data.Word                                ( Word8             )
import Brick                                    ( (<+>), (<=>)      )
import Brick.Widgets.Border                     ( vBorder
                                                , border
                                                , hBorder           )
import Model.Debugger.Debugger                  ( getPosition       )
import View.Core                                ( addNumberedRow
                                                , renderTitle       )
import Model.Utilities                          ( chunksOf
                                                , slice
                                                , toAscii
                                                , toAsciiAll
                                                , toHex
                                                , toDec             )

-- =============================================================== --
-- Assembling the main debugger UI

drawUI :: T.Debugger -> [ B.Widget T.WgtName ]
-- ^Route the rendering result based on the debugger mode.
drawUI db = case T.mode db of
                 T.NormalMode       -> drawNormalUI db
                 T.CommandMode      -> drawCommandUI db
                 T.ProcessingMode _ -> drawNormalUI db
                 T.HelpMode cs      -> drawHelpUI cs

drawNormalUI :: T.Debugger -> [ B.Widget T.WgtName ]
-- ^Render the UI under normal mode.
drawNormalUI db = [ B.withAttr "background" $
                    mainWidgets db <=> statusUI db ]

drawCommandUI :: T.Debugger -> [ B.Widget T.WgtName ]
-- ^Render the UI under command mode.
drawCommandUI db = [ B.withAttr "background" $
                     mainWidgets db <=> commandUI db ]

drawHelpUI :: [String] -> [ B.Widget T.WgtName ]
drawHelpUI cs = [ B.withAttr "background" $
                  helpWidgets cs ]

mainWidgets :: T.Debugger -> B.Widget T.WgtName
-- ^Helper function for assembling the widgets that are rendered the
-- same independent of the debugger mode. This is a little
-- complicated in order to get the borders and size policies right.
mainWidgets db = B.joinBorders . border $
    B.hBox [ -- The program UI widget
              B.vBox [ ( renderTitle T.ProgramWgt db )
                    , programUI db ]
           , vBorder
              -- The memory UI widget
           , B.hLimit 12 $
                B.vBox [ B.padRight B.Max ( renderTitle T.MemoryWgt  db )
                       , memoryUI db ]
           , vBorder
              -- The stacked input and output UI widgets
           , B.vBox [ B.padRight B.Max ( renderTitle T.OutputWgt db )
                    , outputUI db
                    , hBorder
                    , B.padRight B.Max ( renderTitle T.InputWgt  db )
                    , inputUI db ]
           ]

-- =============================================================== --
-- Rendering the UI for displaying the program code

programUI :: T.Debugger -> B.Widget T.WgtName
-- ^The code is rendered as standard BF broken up into fixed lengths
-- of BF tokens given the debugger state. To make rendering more
-- efficient, only the lines currently visible are rendered.
programUI db = let m = length . show . Vec.length . T.program $ db
               in  B.padBottom B.Max
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
    | pos == getPosition db        = B.withAttr "focus"     . B.str . show $ x
    | pos == T.cursor db           = B.withAttr "cursor"    . B.str . show $ x
    | Set.member pos (T.breaks db) = B.withAttr "break"     . B.str . show $ x
    | highlightPair                = B.withAttr "highlight" . B.str . show $ x
    | otherwise                    = B.str . show $ x
    where highlightPair = case T.program db Vec.! T.cursor db of
                               T.DBOpenLoop n  -> n == pos
                               T.DBCloseLoop n -> n == pos
                               _               -> False

-- =============================================================== --
-- Rendering the UI for displaying the memory/tape state

memoryUI :: T.Debugger -> B.Widget T.WgtName
-- ^Memory is renderd with each position on its own line. To make
-- rendering more efficient, only the visible regions are rendered.
memoryUI db = let m = length . show . F.length . T.memory . T.computer $ db
              in  B.padBottom B.Max
                  . foldr ( addNumberedRow m ) B.emptyWidget
                  . slice (T.memView db)
                  . zip [0..]
                  . formatMemory $ db

formatMemory :: T.Debugger -> [B.Widget T.WgtName]
-- ^Format each memory value and highlight the focus.
formatMemory db = inBack ++ [inFocus] ++ inFront
       where (T.Tape xs u ys) = T.memory . T.computer $ db
             inBack           = map ( B.str . format ) . reverse $ xs
             inFocus          = B.withAttr "focus" . B.str . format $ u
             inFront          = map ( B.str . format ) $ ys
             format           = case T.memFormat db of
                                     T.Asc -> toAsciiAll
                                     T.Dec -> show
                                     T.Hex -> toHex

-- =============================================================== --
-- Rendering the UI for displaying the input and output UIs
-- In contrast to the program and memory UIs the input and output
-- UIs are fully rendered using scrollable viewports. It may be best
-- in the future to change this, but it is working reasonably well.
-- Rendering the input and output UIs is otherwise identical.

inputUI :: T.Debugger -> B.Widget T.WgtName
inputUI db = B.viewport T.InputWgt B.Both
             . dataUI ( T.inFormat db )
             . T.input . T.computer $ db

outputUI :: T.Debugger -> B.Widget T.WgtName
outputUI db = B.viewport T.OutputWgt B.Both
              . dataUI ( T.outFormat db )
              . T.output . T.computer $ db

dataUI :: T.DataFormat -> BS.ByteString -> B.Widget T.WgtName
-- ^Common rendering operatins for both the input and output UIs.
-- When testing, it was found that rendering ascii outputs was very
-- efficient and did not result in lags. In contrast, it was much
-- more difficult to render hexidecimal and decimal formats whith 8
-- bytes displayed per line. This was true even when essentially the
-- same rendering pipelines were used. Much better rendering
-- efficiency was possible with much less lag when more widgets are
-- rendered per line. Hence, 16 bytes per line is used in the decimal
-- and hexidecimal rendering functions.
dataUI fmt bs
    | BS.null bs = B.str $ "<no data>"
    | otherwise  = w
    where w = case fmt of
                   T.Dec -> wrapWith 16 toDec bs
                   T.Hex -> wrapWith 16 toHex bs
                   T.Asc -> B.vBox . map B.str . lines
                            . concatMap toAscii
                            . BS.unpack $ bs

wrapWith :: Int -> (Word8 -> String) -> BS.ByteString -> B.Widget T.WgtName
-- ^Map a bytestring bs to a widget with with n bytes rendered per
-- line separated by a single space given the rendering function f.
-- Each line is number by the index of the first byte in the line.
wrapWith n f bs = let m = length . show . (n*) $ quot (BS.length bs) n
                  in  B.vBox
                      . map B.str
                      . zipWith ( formatLine m f ) [0,n..]
                      . chunksOf n
                      . BS.unpack $ bs

formatLine :: Int -> (Word8 -> String) -> Int -> [Word8] -> String
-- ^Given a label width m, a label number n and a list of bytes,
-- render each byte to a string using rendering function f and
-- concatenate them separated by a single space such that the string
-- begins with the label number fit into m spaces.
formatLine m f n xs = rightPadStr m (show n) ++ ' ' : ys
    where ys = intercalate " " . map f $ xs

rightPadStr :: Int -> String -> String
-- ^Pad a string on the right with spaces to a length n.
rightPadStr n s = s ++ replicate ( n - length s ) ' '

-- =============================================================== --
-- Rendering the status UI for displaying messages and the command UI
-- for reading user commands.

statusUI :: T.Debugger -> B.Widget T.WgtName
statusUI db
    | null msg  = B.str $ " "
    | otherwise = B.str $ msg
    where msg = T.message db

commandUI :: T.Debugger -> B.Widget T.WgtName
commandUI db = ( B.withAttr "background" $ B.str ":" )
               <+> ( renderEditor (B.str . unlines) True . T.commandEdit $ db )

-- =============================================================== --
-- Rendering the help UI for displaying help and other information

helpWidgets :: [String] -> B.Widget T.WgtName
helpWidgets _ = B.viewport T.HelpWgt B.Both
                . B.txt $ "Help is still being implemented.\nEsc to return."
