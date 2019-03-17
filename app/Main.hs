module Main where

import qualified Data.ByteString as B
import qualified Model.Types     as T
import System.Environment               ( getArgs       )
import Controller                       ( execute
                                        , formatOutput
                                        , getScript     )

main :: IO ()
main = do
    script <- getArgs >>= getScript
    case script >>= execute B.empty of
         Left e  -> putStrLn $ "Error: " ++ e
         Right c -> let result = formatOutput . T.output $ c
                    in  putStrLn result
