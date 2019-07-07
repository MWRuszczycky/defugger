{-# LANGUAGE OverloadedStrings #-}

module ModelTest.DebuggerTest.ExecuteTests
    ( spec
    ) where

-- =============================================================== --
-- Tests for executing scripts by the Debugger.                    --
-- =============================================================== --

import qualified Model.Types              as T
import qualified Model.Debugger.Execute   as DE
import qualified Model.Debugger.Debugger  as D
import qualified UtilitiesTest            as U
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
--  Helpers

nextDebugger :: (T.Debugger -> T.Debugger) -> Int -> T.Debugger -> IO T.Debugger
-- ^Advance a debugger by iterating an operation on it.
nextDebugger go n db = pure $ iterate go db !! n

checkEmptyHist :: T.Debugger -> IO T.Debugger
checkEmptyHist db = do (toList . T.history) db `shouldBe` [0]
                       pure db

checkHistLength :: Int -> T.Debugger -> IO T.Debugger
checkHistLength n db = do (length . T.history) db `shouldBe` n
                          pure db

-- =============================================================== --
-- Tests

---------------------------------------------------------------------
-- Full execution and reversion of scripts

testFull101 :: IO ()
testFull101 =
    U.newDebugger (Just "HelloWorld.bf") Nothing
    >>= nextDebugger DE.jumpForward 1
    >>= U.checkDebugger "" "Hello World!\n" "0 0 72 100 87 33 [10]" 107
    >>= nextDebugger DE.jumpBackward 1
    >>= U.checkDebugger "" "" "[0]" 0
    >>  pure ()

testFull102 :: IO ()
testFull102 =
    U.newDebugger (Just "WriteHelloWorld.bf") (Just "WriteHelloWorld.txt")
    >>= nextDebugger DE.jumpForward 1
    >>= U.checkDebugger "" "Hello World!\n"
                        "72 101 108 108 111 32 87 111 114 108 100 33 [10]" 63
    >>= nextDebugger DE.jumpBackward 1
    >>= U.checkDebugger "Hello World!\n" "" "[0]" 0
    >>  pure ()

---------------------------------------------------------------------
-- Jumping and stepping backwards and forwards through scripts

testJumpsAndSteps101 :: IO ()
testJumpsAndSteps101 =
    -- Set up a new debugger with break points at positions 41 and 61
    U.newDebugger (Just "HelloWorld.bf") Nothing
    >>= nextDebugger D.moveCursorRight 41
    >>= pure . D.setBreakPoint
    >>= nextDebugger D.moveCursorRight 20
    >>= pure . D.setBreakPoint
    -- Run the checks
    >>= nextDebugger DE.jumpForward 1
    >>= U.checkDebugger "" "" "8 0 9 13 11 [4]" 41
    >>= nextDebugger DE.stepForward  30
    >>= U.checkDebugger "" "" "7 4 [10] 13 11 4 1" 17
    >>= nextDebugger DE.stepBackward 41
    >>= U.checkDebugger "" "" "8 1 [8] 12 12 4" 31
    >>= nextDebugger DE.jumpForward  9
    >>= U.checkDebugger "" "He" "0 0 72 [105] 88 32 8" 61
    >>= nextDebugger DE.stepBackward 8
    >>= U.checkDebugger "" "H" "0 0 72 [104] 88 32 8" 53
    >>= nextDebugger DE.jumpBackward 2
    >>= U.checkDebugger "" "" "2 0 63 91 77 [28] 6" 41
    >>= nextDebugger DE.jumpBackward 7
    >>= U.checkDebugger "" "" "[0]" 0
    >>= nextDebugger DE.jumpForward  10
    >>= U.checkDebugger "" "Hello World!\n" "0 0 72 100 87 33 [10]" 107
    >>  pure ()

testJumpsAndSteps102 :: IO ()
testJumpsAndSteps102 =
    -- Set up the debugger with breaks at positions 13 and 44
    U.newDebugger (Just "WriteHelloWorld.bf") (Just "WriteHelloWorld.txt")
    >>= nextDebugger D.moveCursorRight 13
    >>= pure . D.setBreakPoint
    >>= nextDebugger D.moveCursorRight 31
    >>= pure . D.setBreakPoint
    -- Run the checks
    >>= nextDebugger DE.jumpForward 1
    >>= U.checkDebugger "orld!\n" "" "72 101 108 108 111 32 [87]" 13
    >>= nextDebugger DE.stepForward 11
    >>= U.checkDebugger "\n" ""
                        "72 101 108 108 111 32 87 111 114 108 100 33 [0]" 24
    >>= nextDebugger DE.stepBackward 16
    >>= U.checkDebugger "o World!\n" "" "72 101 108 108 [0]" 8
    >>= nextDebugger DE.jumpForward 2
    >>= U.checkDebugger "" "Hell"
                        "72 101 108 [108] 111 32 87 111 114 108 100 33 10" 44
    >>= nextDebugger DE.stepBackward 3
    >>= U.checkDebugger "" "He"
                        "72 101 [108] 108 111 32 87 111 114 108 100 33 10" 41
    >>= nextDebugger DE.jumpBackward 2
    >>= U.checkDebugger "Hello World!\n" "" "[0]" 0
    >>  pure ()

---------------------------------------------------------------------
-- Making sure you cannot get outside the script

testEndPoints101 :: IO ()
testEndPoints101 =
    U.newDebugger (Just "HelloWorld.bf") Nothing
    -- Try to step backward through the start
    >>= nextDebugger DE.stepBackward 10
    >>= U.checkDebugger "" "" "[0]" 0
    >>= checkEmptyHist
    -- Try to jump backward through the start
    >>= nextDebugger DE.jumpBackward 10
    >>= U.checkDebugger "" "" "[0]" 0
    >>= checkEmptyHist
    -- Jump to the end
    >>= nextDebugger DE.jumpForward 1
    -- Try to step forward through the end
    >>= ( \ db -> let n = length . T.history $ db
                  in  nextDebugger DE.stepForward 10 db
                      >>= U.checkDebugger "" "Hello World!\n"
                                          "0 0 72 100 87 33 [10]" 107
                      >>= checkHistLength n )
    -- Try to jump forward through the end
    >>= ( \ db -> let n = length . T.history $ db
                  in  nextDebugger DE.jumpForward 10 db
                      >>= U.checkDebugger "" "Hello World!\n"
                                          "0 0 72 100 87 33 [10]" 107
                      >>= checkHistLength n )
    -- Jump back to the start
    >>= nextDebugger DE.jumpBackward 10
    >>= U.checkDebugger "" "" "[0]" 0
    >>= checkEmptyHist
    >>  pure ()
