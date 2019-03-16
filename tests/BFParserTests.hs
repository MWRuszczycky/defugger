{-# LANGUAGE OverloadedStrings #-}

-- Do not add a module declaration or it will fail to compile

-- =============================================================== --
-- This is the bfparser-test test-suite for testing the BF parser
-- defined in the Parser module.
-- =============================================================== --

import qualified Data.Text  as Tx
import qualified Types      as T
import Data.Text                    ( Text      )
import Parser                       ( parse     )
import Test.Hspec                   ( Spec (..)
                                    , describe
                                    , hspec
                                    , it
                                    , shouldBe  )

main :: IO ()
main = hspec $ do
    describe "Testing the BFParser" $ do
        it "Correctly parses valid programs with dictionary 1" $ do
            testValid bfDict1 testProgV101In testProgV101Out
            testValid bfDict1 testProgV102In testProgV102Out
            testValid bfDict1 testProgV103In testProgV103Out
            testValid bfDict1 testProgV104In testProgV104Out
        it "Correctly handles invalid programs with dictionary 1" $ do
            testInvalid bfDict1 testProgI101In testProgI101Out
            testInvalid bfDict1 testProgI102In testProgI102Out
            testInvalid bfDict1 testProgI103In testProgI103Out
            testInvalid bfDict1 testProgI104In testProgI104Out
            testInvalid bfDict1 testProgI105In testProgI105Out
            testInvalid bfDict1 testProgI106In testProgI106Out
            testInvalid bfDict1 testProgI107In testProgI107Out
            testInvalid bfDict1 testProgI108In testProgI108Out
            testInvalid bfDict1 testProgI109In testProgI109Out
            testInvalid bfDict1 testProgI110In testProgI110Out


testValid :: T.Dictionary -> Text -> Text -> IO ()
testValid dict s t = shouldBe ( Tx.pack . format <$> parse dict s )
                              ( Right t )

testInvalid :: T.Dictionary -> Text -> Text -> IO ()
testInvalid dict s t = shouldBe ( format <$> parse dict s )
                                ( Left . Tx.unpack $ t    )

format :: T.Program -> String
format = concatMap go
    where go (T.WhileLoop p) = "[" ++ concatMap go p ++ "]"
          go s               = show s

---------------------------------------------------------------------
-- Dictionary 1

bfDict1 :: T.Dictionary
bfDict1 = T.dictionary [ ( T.BFGT,    [">"] )
                       , ( T.BFLT,    ["<"] )
                       , ( T.BFPlus,  ["+"] )
                       , ( T.BFMinus, ["-"] )
                       , ( T.BFDot,   ["."] )
                       , ( T.BFComma, [","] )
                       , ( T.BFStart, ["["] )
                       , ( T.BFStop,  ["]"] )
                       , ( T.BFHash,  ["#"] )
                       ]

-- Valid Scripts

testProgV101In, testProgV101Out :: Text
testProgV101In  = "++> ++[<+>-]-->> # a comment \n <<[<>- -++[+<->+]]->+"
testProgV101Out = "++>++[<+>-]-->>#\\n<<[<>--++[+<->+]]->+"

testProgV102In, testProgV102Out :: Text
testProgV102In  = "[++> \t\t. ,++[[<+>-]-->]> # ..<>+ \n\n . <<[# xx \n <>- -++[+<->+]]->+]"
testProgV102Out = "[++>.,++[[<+>-]-->]>#\\n.<<[#\\n<>--++[+<->+]]->+]"

testProgV103In, testProgV103Out :: Text
testProgV103In  = "[++> \t\t. ,++[[<+>-]-->]> # ..<>+ \n\n . <<[# xx \n <>- -++[+<->+]]->+]\n\n"
testProgV103Out = "[++>.,++[[<+>-]-->]>#\\n.<<[#\\n<>--++[+<->+]]->+]"

testProgV104In, testProgV104Out :: Text
testProgV104In  = "[++> \t\t. ,++[[<+>-\n]-->]> # ..<>+ \n\n . <<[# xx \n <>- -++[+<->+]]->+]"
testProgV104Out = "[++>.,++[[<+>-]-->]>#\\n.<<[#\\n<>--++[+<->+]]->+]"

-- Invalid Scripts

testProgI101In, testProgI101Out :: Text
testProgI101In  = "++> ++e[<+>-]--# xx \n>> # a comment \n <<[<>- # yy \n -++[+<->+]]->+"
testProgI101Out = "Line 1: Unrecognized token: e"

testProgI102In, testProgI102Out :: Text
testProgI102In  = "++> ++   e[<+>-]--# xx \n>> # a comment \n <<[<>- # yy \n -++[+<->+]]->+"
testProgI102Out = "Line 1: Unrecognized token: e"

testProgI103In, testProgI103Out :: Text
testProgI103In  = "++> ++[<+>-]--# xx \n>> y # a comment \n <<e[<>- # yy \n -++ e[+<->+]]->+"
testProgI103Out = "Line 2: Unrecognized token: y"

testProgI104In, testProgI104Out :: Text
testProgI104In  = "++> ++[<+>-]--# xx \ny>> # a comment \n <<e[<>- # yy \n -++ e[+<->+]]->+"
testProgI104Out = "Line 2: Unrecognized token: y"

testProgI105In, testProgI105Out :: Text
testProgI105In  = "++> ++[<+>-]--# xx \n \ty>> # a comment \n <<e[<>- # yy \n -++ e[+<->+]]->+"
testProgI105Out = "Line 2: Unrecognized token: y"

testProgI106In, testProgI106Out :: Text
testProgI106In  = "++> ++\ny[<+>-]--# xx \n>> # a comment \n <<e[<>- # yy \n -++ e[+<->+]]->+"
testProgI106Out = "Line 2: Unrecognized token: y"

testProgI107In, testProgI107Out :: Text
testProgI107In  = "++> ++[<+>-]--# xx \n>> # a comment \n <<[<>- # yy \n -++[+<->+]] x e ->+"
testProgI107Out = "Line 4: Unrecognized token: x"

testProgI108In, testProgI108Out :: Text
testProgI108In  = "++> ++[<+>-]--# xx \n>> # a comment \n<<[<>-]] # yy \n -++[+<->+]]x e->+"
testProgI108Out = "Line 3: Unpaired close-brace for while-loop"

testProgI109In, testProgI109Out :: Text
testProgI109In  = "++> ++[<+>-]--# xx \n>> # a comment \n]<<[<>- # yy \n -++[+<->+]]x e->+"
testProgI109Out = "Line 3: Unpaired close-brace for while-loop"

testProgI110In, testProgI110Out :: Text
testProgI110In  = "++> ++[<+>-]--# xx \n>> # a comment \n<<[<>-x # yy \n -++[+<->+]]x e->+"
testProgI110Out = "Line 3: Cannot parse while-loop"
