# MiniRacket Interpreter Project

## Overview

This project is a Haskell implementation of a subset of the Racket language, called MiniRacket. MiniRacket uses LISP-style prefix notation and supports basic arithmetic, boolean operations, and more. This project includes parsing and evaluation of MiniRacket expressions.

## Getting Started

### Prerequisites

To build and run this project, you need:

- Haskell Platform (includes GHC and Cabal)
- `stack` (Haskell build tool)

### Building the Project

1. Clone the repository:

```sh
git clone <repository_url>
cd MiniRacketProject
```
2. Build the project:
```sh
stack build
```
3. Run the test suite:
```sh
stack test
```

### Example Usage
You can parse and evaluate MiniRacket expressions using the provided functions. Here are some examples of expressions you can parse and evaluate:
- Parsing a number:
```haskell
parseString "1235"
-- Output: Right (LiteralExpr (IntValue 1235),"")
```
- Parsing a boolean:
```haskell
parseString "true"
-- Output: Right (LiteralExpr (BoolValue True),"")
```
- Parsing a mathematical expression:
```haskell
parseString "(+ 3 4)"
-- Output: Right (MathExpr Add [LiteralExpr (IntValue 3), LiteralExpr (IntValue 4)],"")
```
- Parsing a negated atom:
```haskell
parseString "-x"
-- Output: Right (MathExpr Sub [LiteralExpr (IntValue 0), VarExpr "x"],"")
```
- Parsing a variable:
```haskell
parseString "x"
-- Output: Right (VarExpr "x","")
```
- Parsing an if expression:
```haskell
parseString "(if true 1 0)"
-- Output: Right (IfExpr (LiteralExpr (BoolValue True)) (LiteralExpr (IntValue 1)) (LiteralExpr (IntValue 0)),"")
```
- Parsing a let expression: 
```haskell
parseString "(let (x 5) (+ x 3))"
-- Output: Right (LetExpr "x" (LiteralExpr (IntValue 5)) (MathExpr Add [VarExpr "x", LiteralExpr (IntValue 3)]),"")
```
- Parsing a lambda expression:
```haskell
parseString "(lambda (x) x)"
-- Output: Right (LambdaExpr "x" (VarExpr "x"),"")
```
- Parsing an apply expression:
```haskell
parseString "((lambda (x) (+ x 1)) 5)"
-- Output: Right (ApplyExpr (LambdaExpr "x" (MathExpr Add [VarExpr "x", LiteralExpr (IntValue 1)])) (LiteralExpr (IntValue 5)),"")
```
- Evaluating a number:
```haskell
evalString "1235"
-- Output: Right (IntValue 1235)
```
- Evaluating a boolean:
```haskell
evalString "true"
-- Output: Right (BoolValue True)
```
- Evaluating a mathematical expression:
```haskell
evalString "(+ 3 4)"
-- Output: Right (IntValue 7)
```
- Evaluating a comparison expression:
```haskell
evalString "(< 3 4)"
-- Output: Right (BoolValue True)
```
- Evaluating a not expression:
```haskell
evalString "(not true)"
-- Output: Right (BoolValue False)
```
- Evaluating a pair expression:
```haskell
evalString "(cons 1 2)"
-- Output: Right (PairValue (IntValue 1, IntValue 2))
```
- Evaluating a variable expression:
```haskell
evalString "x"
-- Output: Left (NoSymbol "symbol x not found")
```
- Evaluating a let expression:
```haskell
evalString "(let (x 5) (+ x 3))"
-- Output: Right (IntValue 8)
```
- Evaluating an if expression:
```haskell
evalString "(if true 1 0)"
-- Output: Right (IntValue 1)
```
- Evaluating a lambda expression:
```haskell
evalString "(lambda (x) x)"
-- Output: Right (ClosureValue "" "x" (VarExpr "x") [])
```
- Evaluating an application expression:
```haskell
evalString "((lambda (x) (+ x 1)) 5)"
-- Output: Right (IntValue 6)
```
- Evaluating an expression:
```haskell
evalString "(+ 3 4)"
-- Output: Right (IntValue 7)
```

## File Descriptions
### `Environment.hs`

Provides functions and data types to manage variable bindings:

- `emptyEnv`: An empty environment.
- `lookup`: Looks up a value by its name in the environment.
- `bindName`: Binds a name to a value in the environment.

### `Error.hs`

Defines various error types that can occur during parsing and evaluation:

- `ErrorType`: Includes `TypeError`, `EvalError`, `SyntaxError`, `ParseError`, and more.

### `Expr.hs`

Defines the abstract syntax tree (AST) and value types for MiniRacket:

- `Expr`: Includes expression types like `BoolExpr`, `MathExpr`, `CompExpr`, `NotExpr`, `LiteralExpr`, `VarExpr`, `IfExpr`, `LetExpr`, `LambdaExpr` and `ApplyExpr`.
- `Value`: Includes value types like `IntValue`, `BoolValue`, `PairValue`, and `ClosureValue`.

### `Lib.hs`

A placeholder for auxiliary functions and definitions.

### `Parser.hs`

Defines the Parser type and combinators for building parsers:

- `Parser`: A type that represents a parser.
- Various combinators: `item`, `fmap`, `pure`, `<*>`, `>>=`, `empty`, `<|>`, `satisfies`, `char`, `string`, `nat`, `int`, `token`, and more.

### `MiniRacketParser.hs`

Implements parsers for MiniRacket expressions:

- `parseBool`: Parses boolean literals (true, false).
- `parseBoolOp`: Parses boolean operations (and, or).
- `parseMathOp`: Parses mathematical operations (+, -, *, div, mod).
- `parseCompOp`: Parses comparison operations (equal?, <).
- `literal`: Parses literal values.
- `literalExpr`: Parses literal expressions.
- `notExpr`: Parses not expressions.
- `boolExpr`: Parses boolean expressions.
- `mathExpr`: Parses mathematical expressions.
- `compExpr`: Parses comparison expressions.
- `pairExpr`: Parses pair expressions.
- `consExpr`: Parses cons expressions.
- `parseAtom`: Parses atomic expressions (variable, literal, negated atom).
- `negateAtom`: Parses negated atomic expressions.
- `varExpr`: Parses variable expressions.
- `ifExpr`: Parses if expressions.
- `letExpr`: Parses let expressions.
- `lambdaExpr`: Parses lambda expressions.
- `applyExpr`: Parses application expressions.
- `parseExpr`: The main parser function that handles different kinds of expressions.
- `parseString`: A helper function for parsing strings.

### `Eval.hs`
Implements the evaluator for the MiniRacket AST:

- `Evaluator`: A type for evaluators.
- `evalLiteral`: Evaluates literal expressions.
- `evalBoolExpr`: Evaluates boolean expressions.
- `evalMathExpr`: Evaluates mathematical expressions.
- `evalCompExpr`: Evaluates comparison expressions.
- `evalNotExpr`: Evaluates not expressions.
- `evalPairExpr`: Evaluates pair expressions.
- `evalVar`: Evaluates variable expressions.
- `evalLetExpr`: Evaluates let expressions.
- `evalIfExpr`: Evaluates if expressions.
- `evalLambdaExpr`: Evaluates lambda expressions.
- `evalApplyExpr`: Evaluates application expressions.
- `evalExpr`: The main evaluator function.
- `parseAndEval`: Parses and then evaluates a string.
- `evalString`: Evaluates a MiniRacket expression from a string.

### `MiniRacketParserSpec.hs`
Contains test cases for the MiniRacket parsers using `Hspec`

### `EvalSpec.hs`
Contains test cases for the evaluator functions for the MiniRacket AST using `Hspec`

## License
This project is licensed under the MIT License. See the `LICENSE` file for details.