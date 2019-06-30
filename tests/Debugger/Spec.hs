-- Debugger test suite.

import qualified BracketTests
import Test.Hspec ( hspec
                  , describe  )

main :: IO ()
main = hspec $ do
    describe "Bracket-finding" BracketTests.spec
