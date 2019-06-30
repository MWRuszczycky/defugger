-- General test suite.

import qualified InterpreterTests
import qualified ParserTests
import Test.Hspec ( hspec
                  , describe  )

main :: IO ()
main = hspec $ do
    describe "Interpreter" InterpreterTests.spec
    describe "Parser"      ParserTests.spec
