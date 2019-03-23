module StartUp
    ( initApp
    , runCatfk
    , formatOutput
    ) where

import qualified Data.ByteString as BS
import qualified Graphics.Vty    as V
import qualified Brick           as B
import qualified Model.Types     as T
import System.Posix.Env                 ( putEnv        )
import Controller                       ( routeEvent    )

initApp :: B.App T.OuterState e ()
initApp = B.App { B.appDraw         = const [ B.str "Welcome to CatFk!" ]
                , B.appHandleEvent  = routeEvent
                , B.appChooseCursor = B.neverShowCursor
                , B.appStartEvent   = pure
                , B.appAttrMap      = const (B.attrMap V.defAttr [])
                }

runCatfk :: T.OuterState -> IO ()
runCatfk st0 = do
    putEnv $ "TERM=xterm-256color"
    result <- B.defaultMain initApp st0
    case result of
         Left err -> putStrLn $ "Error: " ++ err
         Right st -> putStrLn . formatOutput . T.output . T.computer $ st

formatOutput :: BS.ByteString -> String
formatOutput = map ( toEnum . fromIntegral ) . BS.unpack
