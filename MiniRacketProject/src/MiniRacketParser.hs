module MiniRacketParser where

import Parser
import Expr
import Control.Applicative
import Error ( ErrorType ) 

parseBool :: Parser Bool
parseBool = do
    parseKeyword "true"
    return True
    <|> do
        parseKeyword "false"
        return False

-- parse binary bool operations
parseBoolOp :: Parser BoolOp
parseBoolOp = do
    parseKeyword "and" >> return And
    <|> do
        parseKeyword "or"
        return Or

-- parse math operations and return the MathOp
parseMathOp :: Parser MathOp
parseMathOp = do
    symbol "+" >> return Add
    <|> do
        symbol "-" >> return Sub
    <|> do
        symbol "*" >> return Mul
    <|> do
        string "div" >> return Div
    <|> do
        string "mod" >> return Mod

    

-- parse the comparison operations and return the corresponding CompOp
parseCompOp :: Parser CompOp
parseCompOp = do
    string "equal?" >> return Eq
    <|> do
        symbol "<" >> return Lt


-- a literal in MiniRacket is true, false, or a number
literal :: Parser Value
literal = do 
    IntValue <$> natural 
    <|> 
    BoolValue <$> parseBool 

-- parse a literal expression, which is just a literal
literalExpr :: Parser Expr
literalExpr = LiteralExpr <$> literal

keywordList :: [String]
keywordList = ["false", "true", "not", "and", "or", "div", "mod", "equal?", "if", "let", "lambda"]

-- try to parse a keyword, otherwise it is a variable, this can be
-- used to check if the identifier we see (i.e., variable name) is
-- actually a keyword, which is not legal
parseKeyword :: String -> Parser String
parseKeyword keyword =  do
    name <- identifier
    if name `elem` keywordList && keyword == name
    then return name
    else failParse $ "saw " ++ name ++ ", expected " ++ keyword


notExpr :: Parser Expr
notExpr = do 
    parseKeyword "not"
    NotExpr <$> parseExpr

boolExpr :: Parser Expr
boolExpr = do
    op <- parseBoolOp
    exprs <- some parseExpr
    return $ BoolExpr op exprs

mathExpr :: Parser Expr
mathExpr = do
    op <- parseMathOp
    exprs <- some parseExpr
    return $ MathExpr op exprs

-- a comparison expression is the comparison operator
--   followed by two expressions
compExpr :: Parser Expr
compExpr = do
    op <- parseCompOp
    expr1 <- parseExpr
    expr2 <- parseExpr
    return $ CompExpr op expr1 expr2

pairExpr :: Parser Expr
pairExpr = parseParens $ do
    expr1 <- parseExpr
    symbol "."
    expr2 <- parseExpr
    return $ PairExpr expr1 expr2

-- note that this is syntactic sugar, cons is just replaced by a 
--    PairExpr abstract syntax tree 
consExpr :: Parser Expr 
consExpr = do 
    symbol "cons"
    expr1 <- parseExpr 
    PairExpr expr1 <$> parseExpr 

parseParens :: Parser a -> Parser a
parseParens p = do
    symbol "("
    x <- p
    symbol ")"
    return x

-- a helper function for testing parsing
--   To use simply type:
--      parseString "5" 
--   this will use the parseExpr Parser to parse the contents of str
parseString :: String -> Either ErrorType (Expr, String) 
parseString str = parse parseExpr str

-- TODO: Add the following to MiniRacketParser.hs
-- Beginning of additions to MiniRacketParser.hs for Part 2 of the
--   MiniRacketProject

--   an atom is either a var, a literal, or a negated atom
parseAtom :: Parser Expr
parseAtom = do
    varExpr
    <|>
    negateAtom



-- negate an atom, we actually only have one choice here. Our
-- parsing already correctly handles negative numbers, and we
-- cannot have negative boolean values. This leaves variables,
-- and for this we build a MathExpr around the VarExpr.
negateAtom :: Parser Expr
negateAtom = do
    symbol "-"
    var <- parseAtom
    return $ MathExpr Sub [LiteralExpr (IntValue 0), var]

-- parse a var expression, here we need to make sure that
-- the identifier is *not* a keyword before accepting it
-- i.e., we fail the parse if it is
varExpr :: Parser Expr
varExpr = do
    v <- identifier
    if v `elem` keywordList
    then failParse (v ++ " is a keyword, it cannot be used as a variable")
    else return (VarExpr v)


-- parse an if-expression, which begins with the keyword if,
-- and is followed by three expressions
ifExpr :: Parser Expr
ifExpr = do
    parseKeyword "if"
    IfExpr <$> parseExpr <*> parseExpr <*> parseExpr

-- a let expression begins with the keyword let, followed by
-- left parenthesis, then an identifier for the name 
-- to be bound, an expression to bind to that name, and a right
-- parenthesis, and then the body of the let expression
letExpr :: Parser Expr
letExpr = do
    parseKeyword "let"
    symbol "("
    var <- varExpr
    case var of
        VarExpr argname -> do
            v <- parseExpr
            symbol ")"
            body <- parseExpr
            return (LetExpr argname v body)
        _ -> failParse "expected var name in let binding"

-- End of additions to MiniRacketParser.hs for Part 2 of the
--   MiniRacketProject

-- Beginning of additions to MiniRacketParser.hs for Part 3 of the
--   MiniRacketProject

-- parse a lambda expression which is a lambda, argument, 
-- and body, with proper parenthesis around it
lambdaExpr :: Parser Expr
lambdaExpr = do
    parseKeyword "lambda"
    symbol "("
    arg <- identifier
    symbol ")"
    body <- parseExpr
    return (LambdaExpr arg body)

-- This expression consists of a function which is being applied to 
--   a parameter expression.
applyExpr :: Parser Expr
applyExpr = do
    lamFunc <- parseExpr
    ApplyExpr lamFunc <$> parseExpr

-- End of additions to MiniRacketParser.hs for Part 3 of the
--   MiniRacketProject

-- the main parsing function which alternates between all 
-- the options for possible expressions
parseExpr :: Parser Expr
parseExpr = do
    parseAtom
    <|> parseParens (
        notExpr
        <|> boolExpr
        <|> mathExpr
        <|> compExpr
        <|> pairExpr
        <|> consExpr
        <|> lambdaExpr
        <|> applyExpr
        <|> ifExpr
        <|> letExpr
    )
    <|> literalExpr