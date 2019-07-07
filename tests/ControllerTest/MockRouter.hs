module ControllerTest.MockRouter
    ( handleAsSimpleIO
    , handleAsError
    ) where

import qualified Model.Types                as T
import qualified Brick.Widgets.Edit         as Br
import qualified Controller.CommandBindings as CB
import Control.Monad.Except                       ( runExceptT )

-- =============================================================== --
-- Mocking command handling in Controller to bypass the Brick
-- runtime system.

mockIOHandler :: ( T.Debugger -> T.ErrorIO T.Debugger)
                 -> ( T.Debugger -> IO T.Debugger )
mockIOHandler f db = runExceptT ( f db' ) >>= pure . either ( err db' ) id
    where err dbx m = dbx { T.message = m }
          db'       = db { T.commandEdit = Br.editor T.CommandWgt (Just 1) ""
                         , T.mode        = T.NormalMode
                         }

handleAsError :: [String] -> T.Debugger -> IO T.Debugger
handleAsError commands db = do
    case CB.parseCommand . words . unlines $ commands of
         T.PureCmd _      -> error "Expected ErrorCmd command got PureCmd"
         T.ComplexIOCmd _ -> error "Expected ErrorCmd command got ComplexIOCmd"
         T.QuitCmd        -> error "Expected ErrorCmd command got QuitCmd"
         T.SimpleIOCmd _  -> error "Expected ErrorCmd command got SimpleIOCmd"
         T.TandemCmd _    -> error "Expected ErrorCmd command got TandemCmd"
         T.HScrollCmd _ _ -> error "Expected ErrorCmd command got HScrollCmd"
         T.VScrollCmd _ _ -> error "Expected ErrorCmd command got VScrollCmd"
         T.ErrorCmd e     -> pure $ db { T.message = e }

handleAsSimpleIO :: [String] -> T.Debugger -> IO T.Debugger
handleAsSimpleIO commands db = do
    case CB.parseCommand . words . unlines $ commands of
         T.PureCmd _      -> error "Expected SimpleIO command got PureCmd"
         T.ComplexIOCmd _ -> error "Expected SimpleIO command got ComplexIOCmd"
         T.ErrorCmd _     -> error "Expected SimpleIO command got ErrorCmd"
         T.QuitCmd        -> error "Expected SimpleIO command got QuitCmd"
         T.TandemCmd _    -> error "Expected SimpleIO command got TandemCmd"
         T.HScrollCmd _ _ -> error "Expected SimpleIO command got HScrollCmd"
         T.VScrollCmd _ _ -> error "Expected SimpleIO command got VScrollCmd"
         T.SimpleIOCmd f  -> mockIOHandler f db
