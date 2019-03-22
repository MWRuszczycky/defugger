module Main where

import qualified Data.ByteString as BS
import qualified Model.Types     as T
import qualified Graphics.Vty    as V
import qualified Brick           as B
import System.Environment               ( getArgs       )
import Controller                       ( execute       )
import Loader                           ( formatOutput
                                        , getScript     )

main :: IO ()
main = do
    script <- getArgs >>= getScript
    case script >>= execute BS.empty of
         Left e  -> putStrLn $ "Error: " ++ e
         Right c -> putStrLn . formatOutput . T.output $ c

initialize :: B.App T.Computer e ()
initialize = B.App { B.appDraw         = const []
                   , B.appHandleEvent  = B.resizeOrQuit
                   , B.appChooseCursor = B.neverShowCursor
                   , B.appStartEvent   = pure
                   , B.appAttrMap      = const (B.attrMap V.defAttr [])
                   }
