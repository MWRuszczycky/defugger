-- Debugger test suite.

import qualified BracketTests
import qualified ExecuteTests
import Test.Hspec ( hspec
                  , describe  )

main :: IO ()
main = hspec $ do
    describe "Debugger: Bracket-finding" BracketTests.spec
    describe "Debugger: Execute"         ExecuteTests.spec
