module MiniRacketParserSpec where 

import Test.Hspec
import Parser
import Expr 
import MiniRacketParser
import Error

type ParseResult = Either ErrorType (Expr, String)

expr :: Either ErrorType (a2, b) -> a2
expr (Right (e, _)) = e 
expr (Left (SyntaxError msg)) = error msg
expr (Left (ParseError msg)) = error msg
expr (Left NoParse) = error "no matching parse"
expr _ = error "expr in MiniRacketParser.hs is not fully implemented yet..."

spec :: Spec 
spec = do 
    describe "parse literals" $ do
        it "parses number: 1235" $ 
            parseString "1235" `shouldBe` Right (LiteralExpr (IntValue 1235),"")
        it "parses negative numbers: -12235" $
            parseString "-12235" `shouldBe` Right (LiteralExpr (IntValue (-12235)), "")
        it "parses true" $
            parseString "true" `shouldBe` Right (LiteralExpr (BoolValue True), "")
        it "parses false" $
            parseString "false" `shouldBe` Right (LiteralExpr (BoolValue False), "")
    describe "parse BoolOp" $ do
        it "parses BoolOp: and" $
            parse parseBoolOp "and" `shouldBe` Right ((And), "")
        it "parses BoolOp: or" $
            parse parseBoolOp "or" `shouldBe` Right ((Or), "")
        it "parses BoolOp: and with nested expressions" $
            parseString "(and true (or false true))" `shouldBe` Right (BoolExpr And [LiteralExpr (BoolValue True), BoolExpr Or [LiteralExpr (BoolValue False), LiteralExpr (BoolValue True)]], "")
    describe "parse MathOp" $ do
        it "parses MathOp: +" $
            parse parseMathOp "+" `shouldBe` Right ((Add), "")
        it "parses MathOp: -" $
            parse parseMathOp "-" `shouldBe` Right ((Sub), "")
        it "parses MathOp: *" $
            parse parseMathOp "*" `shouldBe` Right ((Mul), "")
        it "parses MathOp: div" $
            parse parseMathOp "div" `shouldBe` Right ((Div), "")
        it "parses MathOp: mod" $
            parse parseMathOp "mod" `shouldBe` Right ((Mod), "")
    describe "parse CompOp" $ do
        it "parses CompOp: equal?" $
            parse parseCompOp "equal?" `shouldBe` Right ((Eq), "")
        it "parses CompOp: <" $
            parse parseCompOp "<" `shouldBe` Right ((Lt), "")
        it "parses CompOp: equal? with nested expressions" $
            parseString "(equal? (+ 1 1) 2)" `shouldBe` Right (CompExpr Eq (MathExpr Add [LiteralExpr (IntValue 1), LiteralExpr (IntValue 1)]) (LiteralExpr (IntValue 2)), "")
    describe "parse notExpr" $ do
        it "parses notExpr: not true" $
            parseString "(not true)" `shouldBe` Right (NotExpr (LiteralExpr (BoolValue True)), "")
        it "parses notExpr: not false" $
            parseString "(not false)" `shouldBe` Right (NotExpr (LiteralExpr (BoolValue False)), "")
        it "parses notExpr: with nested expression" $
            parseString "(not (and true false))" `shouldBe` Right (NotExpr (BoolExpr And [LiteralExpr (BoolValue True), LiteralExpr (BoolValue False)]), "")
        it "parses notExpr: with comparison operation" $
            parseString "(not (< 1 2))" `shouldBe` Right (NotExpr (CompExpr Lt (LiteralExpr (IntValue 1)) (LiteralExpr (IntValue 2))), "")
    describe "parse boolExpr" $ do
        it "parses boolExpr: (or true false)" $
            parseString "(or true false)" `shouldBe` Right (BoolExpr Or [LiteralExpr (BoolValue True), LiteralExpr (BoolValue False)], "")
        it "parses boolExpr: (or false false)" $
            parseString "(or false false)" `shouldBe` Right (BoolExpr Or [LiteralExpr (BoolValue False), LiteralExpr (BoolValue False)], "")
        it "parses boolExpr: (and true false)" $
            parseString "(and true false)" `shouldBe` Right (BoolExpr And [LiteralExpr (BoolValue True), LiteralExpr (BoolValue False)], "")
        it "parses boolExpr: (and false false)" $
            parseString "(and false false)" `shouldBe` Right (BoolExpr And [LiteralExpr (BoolValue False), LiteralExpr (BoolValue False)], "")
    describe "parse mathExpr" $ do
        it "parses mathExpr: addition" $
            parseString "(+ 5 3)" `shouldBe` Right (MathExpr Add [LiteralExpr (IntValue 5), LiteralExpr (IntValue 3)], "")
        it "parses mathExpr: multiplication" $
            parseString "(* 2 4)" `shouldBe` Right (MathExpr Mul [LiteralExpr (IntValue 2), LiteralExpr (IntValue 4)], "")
        it "parses mathExpr: subtraction" $
            parseString "(- 7 4)" `shouldBe` Right (MathExpr Sub [LiteralExpr (IntValue 7), LiteralExpr (IntValue 4)], "")
        it "parses mathExpr: division" $
            parseString "(div 8 2)" `shouldBe` Right (MathExpr Div [LiteralExpr (IntValue 8), LiteralExpr (IntValue 2)], "")
    describe "parse compExpr" $ do
        it "parses compExpr: less than operation" $
            parseString "(< 5 10)" `shouldBe` Right (CompExpr Lt (LiteralExpr (IntValue 5)) (LiteralExpr (IntValue 10)), "")
        it "parses compExpr: equal operation" $
            parseString "(equal? 3 3)" `shouldBe` Right (CompExpr Eq (LiteralExpr (IntValue 3)) (LiteralExpr (IntValue 3)), "")
        it "parses compExpr: less than operation with nested expressions" $
            parseString "(< (+ 1 2) (* 2 3))" `shouldBe` Right (CompExpr Lt (MathExpr Add [LiteralExpr (IntValue 1), LiteralExpr (IntValue 2)]) (MathExpr Mul [LiteralExpr (IntValue 2), LiteralExpr (IntValue 3)]), "")
    describe "parse consExpr" $ do
        it "parses consExpr: 1 and 2" $
            parseString "(cons 1 2)" `shouldBe` Right (PairExpr (LiteralExpr (IntValue 1)) (LiteralExpr (IntValue 2)), "")
        it "parses consExpr: with nested expressions" $
            parseString "(cons (+ 1 2) (- 3 4))" `shouldBe` Right (PairExpr (MathExpr Add [LiteralExpr (IntValue 1), LiteralExpr (IntValue 2)]) (MathExpr Sub [LiteralExpr (IntValue 3), LiteralExpr (IntValue 4)]), "")
        it "parses consExpr: with boolean values" $
            parseString "(cons true false)" `shouldBe` Right (PairExpr (LiteralExpr (BoolValue True)) (LiteralExpr (BoolValue False)), "")

