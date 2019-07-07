-- Controller test suite.

import qualified ControllerTest.CommandBindingsTest as CommandBindings
import Test.Hspec ( hspec
                  , describe  )

main :: IO ()
main = hspec $ do
    describe "Controller" $ do
        describe "CommandBindings" CommandBindings.spec
