-- Model test suite.

import qualified ModelTest.InterpreterTests   as Interpreter
import qualified ModelTest.ParserTests        as Parser
import qualified ModelTest.DebuggerTest.Spec  as Debugger
import Test.Hspec ( hspec
                  , describe  )

main :: IO ()
main = hspec $ do
    describe "Model" $ do
        describe "Interpreter" Interpreter.spec
        describe "Parser"      Parser.spec
        describe "Debugger"    Debugger.spec
