{-# LANGUAGE OverloadedStrings #-}

module ExecuteTests
    ( spec
    ) where

-- =============================================================== --
-- Tests for executing scripts by the Debugger.                    --
-- =============================================================== --

import qualified Model.Types              as T
import qualified Model.Debugger.Debugger  as D
import qualified TestUtilities            as TU
import Data.Foldable                             ( toList     )
import Test.Hspec                                ( Spec
                                                 , describe
                                                 , it
                                                 , shouldBe   )

spec :: Spec
spec = do
    describe "BF statement execution/reversion of HelloWorld.bf" $ do
        it "Correctly executes script to completion and reverts"
            testFull101
        it "Correctly executes jumps & steps both back & forth through breaks"
            testJumpsAndSteps101
        it "Does not step back through start or forward through end"
            testEndPoints101
    describe "BF statement execution/reversion of WriteHelloWorld.bf" $ do
        it "Correctly executes script to completion and reverts"
            testFull102
        it "Correctly executes jumps & steps both back & forth through breaks"
            testJumpsAndSteps102

-- =============================================================== --
--  Tests

---------------------------------------------------------------------
-- Full execution and reversion of scripts

testFull101 :: IO ()
testFull101 = do
    db0 <- TU.newDebugger "tests/files/HelloWorld.bf" Nothing
    let db1 = D.jumpForward db0
    TU.checkDebugger db1 "" "Hello World!\n" "0 0 72 100 87 33 [10]" 107
    let db2 = D.jumpBackward db1
    TU.checkDebugger db2 "" "" "[0]" 0

testFull102 :: IO ()
testFull102 = do
    db0 <- TU.newDebugger "tests/files/WriteHelloWorld.bf" $
                          Just "tests/files/WriteHelloWorld.txt"
    let db1 = D.jumpForward db0
    TU.checkDebugger db1 "" "Hello World!\n"
                     "72 101 108 108 111 32 87 111 114 108 100 33 [10]" 63
    let db2 = D.jumpBackward db1
    TU.checkDebugger db2 "Hello World!\n" "" "[0]" 0

---------------------------------------------------------------------
-- Jumping and stepping backwards and forwards through scripts

testJumpsAndSteps101 :: IO ()
testJumpsAndSteps101 = do
    db0 <- TU.newDebugger "tests/files/HelloWorld.bf" Nothing
    -- Set breakpoints at 41 and 61 as the user would have to
    let db1 = D.setBreakPoint $ iterate D.moveCursorRight db0  !! 41
        db2 = D.setBreakPoint $ iterate D.moveCursorRight db1 !! 20
        db3 = D.jumpForward db2
    TU.checkDebugger db3 "" "" "8 0 9 13 11 [4]" 41
    let db4 = iterate D.stepForward db3 !! 30
    TU.checkDebugger db4 "" "" "7 4 [10] 13 11 4 1" 17
    let db5 = iterate D.stepBackward db4 !! 41
    TU.checkDebugger db5 "" "" "8 1 [8] 12 12 4" 31
    let db6 = iterate D.jumpForward db5 !! 9
    TU.checkDebugger db6 "" "He" "0 0 72 [105] 88 32 8" 61
    let db7 = iterate D.stepBackward db6 !! 8
    TU.checkDebugger db7 "" "H" "0 0 72 [104] 88 32 8" 53
    let db8 = iterate D.jumpBackward db7 !! 2
    TU.checkDebugger db8 "" "" "2 0 63 91 77 [28] 6" 41
    let db9 = iterate D.jumpBackward db8 !! 7
    TU.checkDebugger db9 "" "" "[0]" 0
    let db10 = iterate D.jumpForward db9 !! 10
    TU.checkDebugger db10 "" "Hello World!\n" "0 0 72 100 87 33 [10]" 107

testJumpsAndSteps102 :: IO ()
testJumpsAndSteps102 = do
    db0 <- TU.newDebugger "tests/files/WriteHelloWorld.bf" $
                       Just "tests/files/WriteHelloWorld.txt"
    let db1 = D.setBreakPoint $ iterate D.moveCursorRight db0 !! 13
        db2 = D.setBreakPoint $ iterate D.moveCursorRight db1 !! 31
        db3 = D.jumpForward db2
    TU.checkDebugger db3 "orld!\n" ""
                     "72 101 108 108 111 32 [87]" 13
    let db4 = iterate D.stepForward db3 !! 11
    TU.checkDebugger db4 "\n" ""
                     "72 101 108 108 111 32 87 111 114 108 100 33 [0]" 24
    let db5 = iterate D.stepBackward db4 !! 16
    TU.checkDebugger db5 "o World!\n" ""
                     "72 101 108 108 [0]" 8
    let db6 = iterate D.jumpForward db5 !! 2
    TU.checkDebugger db6 "" "Hell"
                     "72 101 108 [108] 111 32 87 111 114 108 100 33 10" 44
    let db7 = iterate D.stepBackward db6 !! 3
    TU.checkDebugger db7 "" "He"
                     "72 101 [108] 108 111 32 87 111 114 108 100 33 10" 41
    let db8 = iterate D.jumpBackward db7 !! 2
    TU.checkDebugger db8 "Hello World!\n" "" "[0]" 0

---------------------------------------------------------------------
-- Making sure you cannot get outside the script

testEndPoints101 :: IO ()
testEndPoints101 = do
    db0 <- TU.newDebugger "tests/files/HelloWorld.bf" Nothing
    let db1 = iterate D.stepBackward db0 !! 10
    TU.checkDebugger db1 "" "" "[0]" 0
    (toList . T.history) db1 `shouldBe` [0]
    let db2 = iterate D.jumpBackward db1 !! 10
    TU.checkDebugger db2 "" "" "[0]" 0
    (toList . T.history) db2 `shouldBe` [0]
    let db3        = D.jumpForward db2
        histLength =  length . T.history $ db3
        db4        = iterate D.jumpForward db3 !! 10
    TU.checkDebugger db4 "" "Hello World!\n" "0 0 72 100 87 33 [10]" 107
    (length . T.history) db4 `shouldBe` histLength
    let db5 = iterate D.stepForward db4 !! 10
    TU.checkDebugger db5 "" "Hello World!\n" "0 0 72 100 87 33 [10]" 107
    (length . T.history) db5 `shouldBe` histLength
    let db6 = D.jumpBackward db5
    TU.checkDebugger db6 "" "" "[0]" 0
    (toList . T.history) db6 `shouldBe` [0]
