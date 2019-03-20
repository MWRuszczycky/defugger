{-# LANGUAGE OverloadedStrings #-}

module Controller
    ( execute
    ) where

import qualified Data.ByteString as B
import qualified Model.Types     as T
import Data.Text                        ( Text          )
import Model.Compiler                   ( runProgram    )
import Model.Parser                     ( parse         )
import Loader                           ( initComputer
                                        , bfDict        )

execute :: B.ByteString -> Text -> Either T.ErrString T.Computer
execute b t = parse bfDict t >>= runProgram ( initComputer b )
