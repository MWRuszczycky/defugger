module ModelTest.DebuggerTest.Spec
    ( spec
    ) where

import qualified ModelTest.DebuggerTest.BracketTests as BracketTests
import qualified ModelTest.DebuggerTest.ExecuteTests as ExecuteTests
import Test.Hspec ( Spec, describe  )

spec :: Spec
spec = do
    describe "Query: Bracket-finding"                BracketTests.spec
    describe "Execute: jumps & steps forward & back" ExecuteTests.spec
