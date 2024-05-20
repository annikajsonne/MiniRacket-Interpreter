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
keywordList = ["false", "true", "not", "and", "or", "div", "mod", "equal?"]

-- try to parse a keyword, otherwise it is a variable, this can be
-- used to check if the identifier we see (i.e., variable name) is
-- actually a keyword, which is not legal
parseKeyword :: String -> Parser String
parseKeyword keyword = do
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

-- the main parsing function which alternates between all
-- the options you have for possible expressions
parseExpr :: Parser Expr
parseExpr = parseParens (
        notExpr
        <|> boolExpr
        <|> mathExpr
        <|> compExpr
        <|> pairExpr
        <|> consExpr
        <|> literalExpr)
    <|> literalExpr

-- a helper function for testing parsing
--   To use simply type:
--      parseString "5" 
--   this will use the parseExpr Parser to parse the contents of str
parseString :: String -> Either ErrorType (Expr, String) 
parseString str = parse parseExpr str