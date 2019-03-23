module View
    ( drawUI
    ) where

import qualified Brick       as B
import qualified Model.Types as T

drawUI :: T.Debugger -> [ B.Widget () ]
drawUI _ = [ B.str "Welcome to Defugger!" ]
