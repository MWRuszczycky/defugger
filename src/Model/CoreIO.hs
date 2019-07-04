module Model.CoreIO
    ( tryReadFile
    , tryReadBytes
    , tryWriteFile
    ) where

import qualified Data.ByteString as BS
import qualified Data.Text.IO    as Tx
import qualified Model.Types     as T
import Data.Text                        ( Text         )
import Control.Monad.Except             ( ExceptT (..) )
import Control.Exception                ( IOException
                                        , catch        )

tryReadFile :: FilePath -> T.ErrorIO Text
tryReadFile fp = ExceptT $ catch ( Right <$> Tx.readFile fp ) hndlErr
    where hndlErr :: IOException -> IO ( Either T.ErrString Text )
          hndlErr = pure . Left . show

tryWriteFile :: FilePath -> Text -> T.ErrorIO ()
tryWriteFile fp t = ExceptT $ catch ( Right <$> Tx.writeFile fp t ) hndlErr
    where hndlErr :: IOException -> IO ( Either T.ErrString () )
          hndlErr = pure . Left . show

tryReadBytes :: FilePath -> T.ErrorIO BS.ByteString
tryReadBytes fp = ExceptT $ catch ( Right <$> BS.readFile fp ) hndlErr
    where hndlErr :: IOException -> IO ( Either T.ErrString BS.ByteString )
          hndlErr = pure . Left . show
