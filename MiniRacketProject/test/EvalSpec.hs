module EvalSpec where


import Test.Hspec
import Parser
import Expr
import MiniRacketParser

import Eval
import Error

type ParseResult = Either ErrorType (Expr, String)

spec :: Spec
spec = do
    describe "evaluating expressions" $ do
        it "evaluates number: 1235" $ 
            evalString "1235" `shouldBe` Right (IntValue 1235)
        it "evaluates negative numbers: -12235" $
            evalString "-12235" `shouldBe` Right (IntValue (-12235))
        it "evaluates true" $
            evalString "true" `shouldBe` Right (BoolValue True)
        it "evaluates false" $
            evalString "false" `shouldBe` Right (BoolValue False)
    describe "evaluating not" $ do 
        it "evaluates 'not true'" $
            evalString "(not true)" `shouldBe` Right (BoolValue False)
        it "evaluates 'not false'" $
            evalString "(not false)" `shouldBe` Right (BoolValue True)
    describe "evaluating and" $ do 
        it "evaluates 'and true false'" $
            evalString "(and true false)" `shouldBe` Right (BoolValue False)
        it "evaluates 'not false false'" $
            evalString "(and true true)" `shouldBe` Right (BoolValue True)
    describe "evaluating or" $ do 
        it "evaluates 'and true false'" $
            evalString "(or true false)" `shouldBe` Right (BoolValue True)
        it "evaluates 'not false false'" $
            evalString "(or false false)" `shouldBe` Right (BoolValue False)
    describe "evaluating math expressions" $ do
        it "evaluates addition (+ 1 3)" $
            evalString "(+ 1 3)" `shouldBe` Right (IntValue 4)
        it "evaluates subtraction (- 4 2)" $
            evalString "(- 4 2)" `shouldBe` Right (IntValue 2)
        it "evaluates multiply (* 5 5)" $
            evalString "(* 5 5)" `shouldBe` Right (IntValue 25)
        it "evaluates divide (div 6 3)" $
            evalString "(div 6 3)" `shouldBe` Right (IntValue 2)
        it "evaluates mod (mod 7 3)" $
            evalString "(mod 7 3)" `shouldBe` Right (IntValue 1)
        