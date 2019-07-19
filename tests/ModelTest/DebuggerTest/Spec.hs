module ModelTest.DebuggerTest.Spec
    ( spec
    ) where

import qualified ModelTest.DebuggerTest.BracketTests as BracketTests
import qualified ModelTest.DebuggerTest.ExecuteTests as ExecuteTests
import qualified ModelTest.DebuggerTest.StateTests   as StateTests
import Test.Hspec ( Spec, describe  )

spec :: Spec
spec = do
    describe "Query: Bracket-finding"                BracketTests.spec
    describe "Execute: Jumps & steps forward & back" ExecuteTests.spec
    describe "State: Computer/debugger encodings"    StateTests.spec
