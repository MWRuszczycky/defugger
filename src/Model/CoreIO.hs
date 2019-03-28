module Model.CoreIO
    ( tryReadFile
    , tryReadBytes
    ) where

import qualified Data.ByteString as BS
import qualified Data.Text.IO    as Tx
import qualified Model.Types     as T
import Data.Text                        ( Text         )
import Control.Monad.Except             ( ExceptT (..) )
import Control.Exception                ( IOException
                                        , catch        )

tryReadFile :: FilePath -> T.ErrorIO Text
tryReadFile fp = ExceptT $ do
    catch ( Right <$> Tx.readFile fp ) hndlErr
    where hndlErr :: IOException -> IO ( Either T.ErrString Text )
          hndlErr = pure . Left . show

tryReadBytes :: FilePath -> T.ErrorIO BS.ByteString
tryReadBytes fp = ExceptT $ do
    catch ( Right <$> BS.readFile fp ) hndlErr
    where hndlErr :: IOException -> IO ( Either T.ErrString BS.ByteString )
          hndlErr = pure . Left . show
