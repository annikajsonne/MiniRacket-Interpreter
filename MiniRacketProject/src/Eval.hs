{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use lambda-case" #-}
module Eval where

import Expr
import Environment
import MiniRacketParser
import Error


import Control.Applicative
import Control.Monad
import Parser


-- we begin with an Expr, and produce a result of Either String (a, Expr). 
-- In the result, String represents an error and contains the error message, 
-- and the tuple (ValueEnv, Expr) contains the current Environment storing (name, value) bindings
-- along with the remaining expression to be evaluated
newtype Evaluator a = E { eval :: (ValueEnv, Expr) -> Either ErrorType (a, (ValueEnv, Expr)) }

-- this is the basic evaluator, it ends when our Abstract Syntax Tree (AST) is an EmptyExpr, 
--   but otherwise, we produce the environment and expr
next :: Evaluator (ValueEnv, Expr)
next = E (\parserState -> case parserState of 
            (_, EmptyExpr) -> Left $ EvalError "no more expressions"
            (env, x) -> Right ((env, x), (env, EmptyExpr)))

-- these next evaluators are primarily for error purposes, you can 
-- call them whenever you want to generate an error
evalError :: ErrorType -> Evaluator a
evalError err = E (\_ -> Left err)

evalNotImplemented :: Evaluator a
evalNotImplemented = evalError EvalNotImplemented 

-- failEval is an EvalError which is a general error for evaluation
failEval :: String -> Evaluator a
failEval msg = evalError $ EvalError msg

-- NoEval is called when there is nothing to evaluate
noEval :: Evaluator a
noEval = evalError NoEval

-- a TypeError is an error when there's a type mismatch discovered during runtime
typeError :: String -> Evaluator a
typeError msg = evalError $ TypeError msg

-- a NoSymbol error is an error when a symbol is looked up in the environment, but not found
noSymbol :: String -> Evaluator a
noSymbol msg = evalError $ NoSymbol msg


-- an evaluator is a Functor
instance Functor Evaluator where
    fmap f e = E (\expr -> case eval e expr of
        Left msg -> Left msg
        Right (v, out) -> Right (f v, out))

-- an evaluator is also an Applicative
instance Applicative Evaluator where
    -- the evaluator will return a successful evaluation with v as the result
    -- of evaluation and expr as the remaining stuff to evaluate
    pure v = E (\expr -> Right (v, expr))

    efuns <*> px = E (\expr -> case eval efuns expr of
        Left msg -> Left msg
        Right (g, out) -> eval (fmap g px) out)

-- and a Monad...
instance Monad Evaluator where
    e >>= f = E (\expr -> case eval e expr of
        Left msg -> Left msg
        Right (v, out) -> eval (f v) out)

-- and an Alternative ... 
instance Alternative Evaluator where
    empty = failEval "no matching evaluation"

    p <|> q = E (\expr -> case eval p expr of
        Left NoEval -> eval q expr
        Left (EvalError _) -> eval q expr
        -- we will pass forward the Lefts otherwise
        otherEval -> otherEval)

-- in fact we're a MonadPlus too, this helps when we don't have
-- an alternative or when simply we use pattern matching on <-, but
-- that fails, this allows the Monad to pass along the failure 
-- instead of generating a pattern-matching failure 
instance MonadPlus Evaluator
instance MonadFail Evaluator where
  fail _ = mzero


evalLiteral :: Evaluator Value
evalLiteral = do
    (env, LiteralExpr val) <- next
    return val

-- this evaluates a list of expressions and returns a list of values
-- by mapping an evaluator function (using <$>) over the list of expressions
evalListOfExprs :: ValueEnv -> [Expr] -> [Either ErrorType Value]
evalListOfExprs env exprs =
    (\expr ->
        case eval evalExpr (env, expr) of
            Right (res, _) -> Right res
            Left msg -> Left msg) <$> exprs


-- evaluates a bool expression, this first evaluates all the 
-- arguments to the bool expression and then uses calcBoolList 
-- to calculate the boolean operation over the arguments. Note that 
-- you must first use evalListOfExprs to evaluate the arguments. Then 
-- you can use calcBoolList with the right op on it
evalBoolExpr :: Evaluator Value
evalBoolExpr = do
    (env, BoolExpr op exprs) <- next
    let values = evalListOfExprs env exprs
    case calcBoolList op values of
        Right val -> return val
        Left err -> evalError err


-- performs the boolean operation on Either String Values where this works on the Values
-- only if the kinds are BoolVal, otherwise we return Left
boolOpVal :: (Bool -> Bool -> Bool) -> Either ErrorType Value -> Either ErrorType Value -> Either ErrorType Value
boolOpVal boolop (Right (BoolValue v1)) (Right (BoolValue v2)) = Right $ BoolValue $ v1 `boolop` v2
boolOpVal _ _ _ = Left $ TypeError "boolean expressions require boolean values"

-- fold over the list using the op, assumes the list has one element
boolOpFold :: Foldable t =>
    (Bool -> Bool -> Bool) -> t (Either ErrorType Value) -> Either ErrorType Value
boolOpFold op = foldr1 (boolOpVal op)

-- determine which bool operation to use to fold with by examining the 
--   kind of BoolOp we were given
calcBoolList :: BoolOp -> [Either ErrorType Value] -> Either ErrorType Value
calcBoolList op lst = case op of
    And -> boolOpFold (&&) lst
    Or -> boolOpFold (||) lst


evalMathExpr :: Evaluator Value
evalMathExpr = do
    (env, MathExpr op exprs) <- next
    let evalArgumentsList = evalListOfExprs env exprs
    case calcMathList op evalArgumentsList of
        Right val -> return val
        Left err -> evalError err

-- evaluates a comparison, specifically equals? and <
evalCompExpr :: Evaluator Value
evalCompExpr = do
    (env, CompExpr op exp1 exp2) <- next
    let eval1 = getValue (eval evalExpr (env, exp1))
    let eval2 = getValue (eval evalExpr (env, exp2))
    case calcCompExpr op eval1 eval2 of
        Right val -> return val
        Left err -> evalError err

-- takes two Either Values and runs the math op on them internally, producing the same type,
-- but failing if either of them is not an IntVal (which are the only things math ops work on)
mathOpVal :: (Integer -> Integer -> Integer) -> Either ErrorType Value -> Either ErrorType Value
    -> Either ErrorType Value
mathOpVal op (Right (IntValue v1)) (Right (IntValue v2)) = Right $ IntValue $ v1 `op` v2
mathOpVal _ _ _ = Left $ TypeError "math expressions require numeric values"

eqOpVal :: (Integer -> Integer -> Bool) -> Either ErrorType Value -> Either ErrorType Value
    -> Either ErrorType Value
--eqOpVal op (Right (BoolValue v1)) (Right (BoolValue v2)) = Right $ BoolValue $ v1 `op` v2 
eqOpVal op (Right (IntValue v1)) (Right (IntValue v2)) = Right $ BoolValue $ v1 `op` v2
eqOpVal _ _ _ = Left $ TypeError "comparisons must be on the same type"

-- takes a list of Either Values and combines them through the operation
mathOpFold :: Foldable t => (Integer -> Integer -> Integer) -> t (Either ErrorType Value)
    -> Either ErrorType Value
mathOpFold op = foldr1 (mathOpVal op)

mathOpFoldl :: Foldable t => (Integer -> Integer -> Integer) -> t (Either ErrorType Value)
    -> Either ErrorType Value
mathOpFoldl op = foldl1 (mathOpVal op)

addValList :: Foldable t => t (Either ErrorType Value) -> Either ErrorType Value
addValList = mathOpFold (+)

subValList :: Foldable t => t (Either ErrorType Value) -> Either ErrorType Value
subValList = mathOpFoldl (-)


mulValList :: Foldable t => t (Either ErrorType Value) -> Either ErrorType Value
mulValList = mathOpFold (*)

divValList :: Foldable t => t (Either ErrorType Value) -> Either ErrorType Value
divValList = mathOpFold (div)

modValList :: Foldable t => t (Either ErrorType Value) -> Either ErrorType Value
modValList = mathOpFold (mod)


calcMathList :: 
    MathOp -> [Either ErrorType Value] -> Either ErrorType Value
calcMathList op lst = case op of
    Add -> addValList lst
    Sub -> subValList lst
    Mul -> mulValList lst
    Div -> divValList lst
    Mod -> modValList lst
    _ -> error "calcMathList not fully Not implemented"


{-
  A somewhat complicated implementation of calcCompExpr mainly because we have 
  so many monadic types that we have to deconstruct for the various cases. Left is
  always a failure, like a type, but Right can then be the different sub-types, like
  Int or Bool, and we can use equal? on int or bools, but not < on bools.  
-}
calcCompExpr :: CompOp -> Either ErrorType Value -> Either ErrorType Value -> Either ErrorType Value
calcCompExpr Eq v@(Right (IntValue _)) v'@(Right (IntValue _)) = Right $ BoolValue $ v == v'
calcCompExpr Eq v@(Right (BoolValue _)) v'@(Right (BoolValue _)) = Right $ BoolValue $ v == v'
calcCompExpr Eq (Right (PairValue (v, v'))) (Right (PairValue (v1, v1'))) =
    case (calcCompExpr Eq (Right v) (Right v1), calcCompExpr Eq (Right v') (Right v1')) of
        (Right (BoolValue True), Right (BoolValue True)) -> Right $ BoolValue True
        (Left _, _) -> Left $ TypeError "equal? can only compare values of the same type"
        (_, Left _) -> Left $ TypeError "equal? can only compare values of the same type"
        (_, _) -> Right $ BoolValue False
calcCompExpr Eq _ _ = Left $ TypeError "equal? can only compare values of the same type"
calcCompExpr Lt (Right (IntValue v)) (Right (IntValue v')) = Right $ BoolValue $ v < v'
calcCompExpr Lt _ _ = Left $ TypeError "< can only compare values of the numbers type"

-- evaluate a Not expression, which should flip the boolean result 
evalNotExpr :: Evaluator Value
evalNotExpr = do
    (env, NotExpr expr) <- next
    case getValue $ eval evalExpr (env, expr) of 
        Right (BoolValue b) -> return (BoolValue (not b))
        Left err -> evalError err

-- evaluates a Pair
evalPairExpr :: Evaluator Value
evalPairExpr = do
    -- extract the current environment and make sure this is a Pair type
    (env, PairExpr e1 e2) <- next
    -- to evaluate a pair, we must evaluate the left and right parts of the pair
    case getValue $ eval evalExpr (env, e1) of 
        Right v1 ->
            case getValue $ eval evalExpr (env, e2) of
                Right v2 -> return $ PairValue (v1, v2)
                Left err -> evalError err
        Left err -> evalError err

-- evaluating an expression returns a value, this is the main
-- entry point for all evaluations, so we alternate between the 
-- different options. Note that depending on what you put first might
-- change how you evaluate your expressions.

-- evaluating an expression returns a value
evalExpr :: Evaluator Value
evalExpr =
    evalBoolExpr
    <|>
    evalNotExpr
    <|>
    evalCompExpr
    <|>
    evalMathExpr
    <|>
    evalLiteral
    <|>
    evalPairExpr
    <|>
    evalVar
    <|>
    evalIfExpr
    <|>
    evalLetExpr
    <|>
    evalLambdaExpr
    <|>
    evalApplyExpr

-- parses the string then evaluates it
parseAndEval :: String -> Either ErrorType (Value, (ValueEnv, Expr))
parseAndEval str = do
    (ast, _) <- parse parseExpr str
    -- here, [] represents the empty environment
    eval evalExpr ([], ast)


-- extract the value from the result, which contains extra stuff we don't need to see
getValue :: Either ErrorType (Value, (ValueEnv, Expr)) -> Either ErrorType Value
getValue (Right (val, _ )) = Right val
getValue (Left err) = Left err


-- Consume a string, parse it, and then try to evaluate it
evalString :: String -> Either ErrorType Value
evalString = getValue . parseAndEval


-- Beginning of additions to Eval.hs for Part 2 of the MiniRacketProject

-- Evaluate a let expression. This requires evaluating the
-- argument to the identifier. Once that is evaluated, we
-- bind the value to the name in a new environment, then
-- we evaluated the body with this new environment
evalLetExpr :: Evaluator Value
evalLetExpr = do
    (env, LetExpr letName valexpr body) <- next
    case getValue (eval evalExpr (env, valexpr)) of
        -- we got a closure from it, but it doesn't have a name, 
        -- so let's add that to the closure as its 'funname' 
        Right (ClosureValue "" argName funBody cenv) ->
            let env' = Environment.bindName letName (ClosureValue letName argName funBody cenv) cenv in
                case getValue (eval evalExpr (env', body)) of
                    Right v -> return v
                    Left err -> evalError err
        Right nameval ->
            case getValue (eval evalExpr (Environment.bindName letName nameval env, body)) of
                Right letval -> return letval
                Left err -> evalError err
        Left err -> evalError err

-- Evaluate an if expression, this requires evaluating
-- the first expression in the if, which is the predicate (boolean value).
-- Only until this returns a value should you evaluate one
-- of the branches. You should NOT evaluate both branches,
-- just the 2nd expression if the test case returns true,
-- and the 3rd expression if the test case returns false
evalIfExpr :: Evaluator Value
evalIfExpr = do
    (env, IfExpr boolexpr texpr fexpr) <- next
    case getValue $ eval evalExpr (env, boolexpr) of 
        Right (BoolValue True) -> do
            case getValue $ eval evalExpr (env, texpr) of
                Right val -> return val
                Left err -> evalError err
        Right (BoolValue False) -> do
            case getValue $ eval evalExpr (env, fexpr) of
                Right val -> return val
                Left err -> evalError err
        Left err -> evalError err

-- Evaluate a Var, this requires looking up the symbol
-- in the current environment. If it's there, we return
-- the value. If it's not, we generate a NoSymbol error
-- via: noSymbol $ "symbol " ++ name ++ " not found"
evalVar :: Evaluator Value
evalVar = do 
    (env, VarExpr name) <- next 
    case (Environment.lookup name env) of
        Just val -> return val
        Nothing -> noSymbol $ "symbol " ++ name ++ " not found" 


-- End of additions to Eval.hs for Part 2 of the MiniRacketProject


-- Beginning of additions to Eval.hs for Part 3 of the MiniRacketProject

-- evaluate a lambda expression, which should result in a ClosureValue, 
-- containing the function name (if there is one), formal parameter name, and body,
-- the ClosureValue can later be used when evaluating an applyExpr. 
-- if function is a totally anonymous function, then "" is the function name, 
-- otherwise if it was built with a let expression, then the let name is the function name.
evalLambdaExpr :: Evaluator Value
evalLambdaExpr = do
    (env, LambdaExpr argName body) <- next
    return $ ClosureValue "" argName body env

-- This is a helper function for evaluating function application expressions
-- callFun expects a closure and a value. Inside the closure, 
-- we find the formal parameter name (which can then be used in the function body). This
-- name is bound to the value in the *Closure's* environment.
-- The body of the function is then evaluated using the Closure's environment.
--
-- For recursion to work, 
-- add the function name to the closure environment also (before you
-- evaluate the expression). This ensures that recursion will work 
-- because if it evaluates the body, it will know that the function 
-- already exists--this call to eval will result in a complicated value
callFun :: Value -> Value -> Either ErrorType Value
callFun c@(ClosureValue funName argName body cenv) argVal =
    let env = Environment.bindName argName argVal cenv
        env' = if funName == "" then env else Environment.bindName funName c env
    in getValue $ eval evalExpr (env', body)
callFun _ _ = error "callFun must have a closure passed to it"

-- Evaluate applyExpr, which is a function call to an argument. 
-- The first expression needs to evaluate to a closure from
-- a lambda expression or a let binding. The second expression
-- is is evaluated to give the resulting value to the function.
-- with this, we then use the callFun function to evalute
-- the function call
evalApplyExpr :: Evaluator Value
evalApplyExpr = do
    (env, ApplyExpr funcExpr argExpr) <- next
    case getValue (eval evalExpr (env, funcExpr)) of
        Right closure@(ClosureValue _ _ _ _) -> do
            case getValue (eval evalExpr (env, argExpr)) of
                Right argVal -> case callFun closure argVal of
                    Right result -> return result
                    Left err -> evalError err
                Left err -> evalError err
        Left err -> evalError err

-- End of additions to Eval.hs for Part 3 of the MiniRacketProject

