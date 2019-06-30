{-# LANGUAGE OverloadedStrings #-}

module BracketTests
    ( spec
    ) where

-- Do not add a module declaration or it will fail to compile

-- =============================================================== --
-- Testing functions associated with bracket-finding               --
-- =============================================================== --

import qualified Data.Text               as Tx
import qualified Data.Vector             as Vec
import qualified Model.Types             as T
import qualified Model.Parser            as P
import qualified Model.Debugger.Debugger as D
import Data.Default                             ( def               )
import Data.Vector                              ( (!)               )
import Data.Text                                ( Text              )
import Test.Hspec                               ( Spec
                                                , describe
                                                , it
                                                , shouldSatisfy
                                                , shouldNotSatisfy
                                                , shouldBe          )

spec :: Spec
spec = do
    bracketFinding101
    bracketFinding102
    bracketFinding103
    bracketFinding201

bracketFinding101 :: Spec
bracketFinding101 = do
    let p = getTestProgram testScript101
        n = Vec.length p - 1
    describe "Testing isBracket with testScript101" $ do
        it "Identifies open and close brackets correctly" $ do
            p ! 9  `shouldSatisfy`    D.isBracket
            p ! 15 `shouldSatisfy`    D.isBracket
            p ! 34 `shouldSatisfy`    D.isBracket
            p ! 46 `shouldSatisfy`    D.isBracket
            p ! 49 `shouldSatisfy`    D.isBracket
        it "Identifies nonbrackets correctly" $ do
            p ! 0  `shouldNotSatisfy` D.isBracket
            p ! 5  `shouldNotSatisfy` D.isBracket
            p ! 12 `shouldNotSatisfy` D.isBracket
            p ! 24 `shouldNotSatisfy` D.isBracket
            p ! 39 `shouldNotSatisfy` D.isBracket
            p ! 48 `shouldNotSatisfy` D.isBracket
            p ! 59 `shouldNotSatisfy` D.isBracket
            p ! 66 `shouldNotSatisfy` D.isBracket
            p ! 107`shouldNotSatisfy` D.isBracket
    describe "Testing getOuterWhile with testScript101 (HelloWorld)" $ do
        it "Works at end points and when exceeding end points" $ do
            D.getOuterWhile 0     p `shouldBe` Nothing
            D.getOuterWhile (-1)  p `shouldBe` Nothing
            D.getOuterWhile n     p `shouldBe` Nothing
            D.getOuterWhile (n+1) p `shouldBe` Nothing
        it "Works when not in a while loop" $ do
            D.getOuterWhile 1  p `shouldBe` Nothing
            D.getOuterWhile 4  p `shouldBe` Nothing
            D.getOuterWhile 4  p `shouldBe` Nothing
            D.getOuterWhile 82 p `shouldBe` Nothing
            D.getOuterWhile 8  p `shouldBe` Nothing
            D.getOuterWhile 50 p `shouldBe` Nothing
        it "Works when on the outermost brackets" $ do
            D.getOuterWhile 9   p `shouldBe` Just (9,49)
            D.getOuterWhile 49  p `shouldBe` Just (9,49)
        it "Works when in the outermost brackets & not on a bracket" $ do
            D.getOuterWhile 12 p `shouldBe` Just (9,49)
            D.getOuterWhile 26 p `shouldBe` Just (9,49)
            D.getOuterWhile 39 p `shouldBe` Just (9,49)
            D.getOuterWhile 39 p `shouldBe` Just (9,49)
            D.getOuterWhile 45 p `shouldBe` Just (9,49)
            D.getOuterWhile 48 p `shouldBe` Just (9,49)
        it "Works when in the outermost brackets & on a open bracket" $ do
            D.getOuterWhile 15 p `shouldBe` Just (9,49)
            D.getOuterWhile 44 p `shouldBe` Just (9,49)
        it "Works when in the outermost brackets & on a close bracket" $ do
            D.getOuterWhile 34 p `shouldBe` Just (9,49)
            D.getOuterWhile 46 p `shouldBe` Just (9,49)

bracketFinding102 :: Spec
bracketFinding102 = do
    let p = getTestProgram testScript102
    describe "Testing isBracket with testScript102 (Adder)" $ do
        it "Identifies open and close brackets correctly" $ do
            p ! 1  `shouldSatisfy`    D.isBracket
            p ! 6  `shouldSatisfy`    D.isBracket
        it "Identifies nonbrackets correctly" $ do
            p ! 0  `shouldNotSatisfy` D.isBracket
            p ! 7  `shouldNotSatisfy` D.isBracket
            p ! 4  `shouldNotSatisfy` D.isBracket
    describe "Testing getOuterWhile with testScript101" $ do
        it "Works at end points and when exceeding end points" $ do
            D.getOuterWhile 0    p `shouldBe` Nothing
            D.getOuterWhile (-1) p `shouldBe` Nothing
            D.getOuterWhile 7    p `shouldBe` Nothing
            D.getOuterWhile 8    p `shouldBe` Nothing
        it "Works when on the outermost brackets" $ do
            D.getOuterWhile 1 p `shouldBe` Just (1,6)
            D.getOuterWhile 6 p `shouldBe` Just (1,6)
        it "Works when in the outermost brackets & not on a bracket" $ do
            D.getOuterWhile 2 p `shouldBe` Just (1,6)
            D.getOuterWhile 4 p `shouldBe` Just (1,6)
            D.getOuterWhile 5 p `shouldBe` Just (1,6)

bracketFinding103 :: Spec
bracketFinding103 = do
    let p = getTestProgram testScript103
    describe "Testing getOuterWhile with testScript103" $ do
        it "Works at end points and when exceeding end points" $ do
            D.getOuterWhile 0    p `shouldBe` Nothing
            D.getOuterWhile (-1) p `shouldBe` Nothing
            D.getOuterWhile 16   p `shouldBe` Nothing
            D.getOuterWhile 20   p `shouldBe` Nothing
        it "Works when on the outermost brackets" $ do
            D.getOuterWhile 1  p `shouldBe` Just (1,  6)
            D.getOuterWhile 6  p `shouldBe` Just (1,  6)
            D.getOuterWhile 10 p `shouldBe` Just (10,15)
            D.getOuterWhile 15 p `shouldBe` Just (10,15)
        it "Works when in the outermost brackets & not on a bracket" $ do
            D.getOuterWhile 4  p `shouldBe` Just (1,6)
            D.getOuterWhile 13 p `shouldBe` Just (10,15)
        it "Works when between the outermost brackets & not on a bracket" $ do
            D.getOuterWhile 8  p `shouldBe` Nothing

bracketFinding201 :: Spec
bracketFinding201 = do
    describe "Testing inSameWhile" $ do
        it "Works with testScript101" $ do
            let p101 = getTestProgram testScript101
            (14,31) `shouldSatisfy`    ( \ (i,j) -> D.inSameWhile i j p101 )
            (31,14) `shouldSatisfy`    ( \ (i,j) -> D.inSameWhile i j p101 )
            (24,45) `shouldSatisfy`    ( \ (i,j) -> D.inSameWhile i j p101 )
            (45,24) `shouldSatisfy`    ( \ (i,j) -> D.inSameWhile i j p101 )
            (9 ,41) `shouldSatisfy`    ( \ (i,j) -> D.inSameWhile i j p101 )
            (41, 9) `shouldSatisfy`    ( \ (i,j) -> D.inSameWhile i j p101 )
            (34,46) `shouldSatisfy`    ( \ (i,j) -> D.inSameWhile i j p101 )
            (46,34) `shouldSatisfy`    ( \ (i,j) -> D.inSameWhile i j p101 )
            (40,40) `shouldSatisfy`    ( \ (i,j) -> D.inSameWhile i j p101 )
            (6 ,22) `shouldNotSatisfy` ( \ (i,j) -> D.inSameWhile i j p101 )
            (22, 6) `shouldNotSatisfy` ( \ (i,j) -> D.inSameWhile i j p101 )
            (39,65) `shouldNotSatisfy` ( \ (i,j) -> D.inSameWhile i j p101 )
            (65,39) `shouldNotSatisfy` ( \ (i,j) -> D.inSameWhile i j p101 )
            ( 6,83) `shouldNotSatisfy` ( \ (i,j) -> D.inSameWhile i j p101 )
            (83, 6) `shouldNotSatisfy` ( \ (i,j) -> D.inSameWhile i j p101 )
            ( 0,56) `shouldNotSatisfy` ( \ (i,j) -> D.inSameWhile i j p101 )
            (56, 0) `shouldNotSatisfy` ( \ (i,j) -> D.inSameWhile i j p101 )
            (107,6) `shouldNotSatisfy` ( \ (i,j) -> D.inSameWhile i j p101 )
            (6,107) `shouldNotSatisfy` ( \ (i,j) -> D.inSameWhile i j p101 )
            (0,107) `shouldNotSatisfy` ( \ (i,j) -> D.inSameWhile i j p101 )
            (107,0) `shouldNotSatisfy` ( \ (i,j) -> D.inSameWhile i j p101 )
            (56,56) `shouldNotSatisfy` ( \ (i,j) -> D.inSameWhile i j p101 )
        it "Works with testScript102" $ do
            let p102 = getTestProgram testScript102
            (4,4) `shouldSatisfy`      ( \ (i,j) -> D.inSameWhile i j p102 )
            (3,4) `shouldSatisfy`      ( \ (i,j) -> D.inSameWhile i j p102 )
            (1,4) `shouldSatisfy`      ( \ (i,j) -> D.inSameWhile i j p102 )
            (4,6) `shouldSatisfy`      ( \ (i,j) -> D.inSameWhile i j p102 )
            (1,6) `shouldSatisfy`      ( \ (i,j) -> D.inSameWhile i j p102 )
            (0,6) `shouldNotSatisfy`   ( \ (i,j) -> D.inSameWhile i j p102 )
            (7,1) `shouldNotSatisfy`   ( \ (i,j) -> D.inSameWhile i j p102 )
            (7,1) `shouldNotSatisfy`   ( \ (i,j) -> D.inSameWhile i j p102 )
            (0,7) `shouldNotSatisfy`   ( \ (i,j) -> D.inSameWhile i j p102 )
            (4,7) `shouldNotSatisfy`   ( \ (i,j) -> D.inSameWhile i j p102 )
            (4,0) `shouldNotSatisfy`   ( \ (i,j) -> D.inSameWhile i j p102 )
        it "Works with testScript103" $ do
            let p103 = getTestProgram testScript103
            ( 3, 4) `shouldSatisfy`    ( \ (i,j) -> D.inSameWhile i j p103 )
            (12,13) `shouldSatisfy`    ( \ (i,j) -> D.inSameWhile i j p103 )
            ( 4,13) `shouldNotSatisfy` ( \ (i,j) -> D.inSameWhile i j p103 )

---------------------------------------------------------------------
-- Helpers

getTestProgram :: Text -> T.DBProgram
getTestProgram = either err id . P.parseDebug def
    where err e = error $ "Test failed: " ++ e

---------------------------------------------------------------------
-- Test scripts

testScript101 :: Text
testScript101 = Tx.intercalate "\n" s
    where s = [ "++++++++[>++++[>++>+++>+++>+<<<<-]>+>+>->>+[<]<-]>>.>"
              , "---.+++++++..+++.>>.<-.<.+++.------.--------.>>+.>++." ]

testScript102 :: Text
testScript102 = "[->+<]"

testScript103 :: Text
testScript103 = "[->+<]+++[->+<]"

