module Model.CoreIO
    ( tryReadFile
    ) where

import qualified Data.Text.IO   as Tx
import qualified Model.Types    as T
import Data.Text                        ( Text         )
import Control.Monad.Except             ( ExceptT (..) )
import Control.Exception                ( IOException
                                        , catch        )

tryReadFile :: FilePath -> ExceptT T.ErrString IO Text
tryReadFile fp = ExceptT $ do
    catch ( Right <$> Tx.readFile fp ) hndlErr
    where hndlErr :: IOException -> IO ( Either T.ErrString Text )
          hndlErr = pure . Left . show
