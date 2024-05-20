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

- `Expr`: Includes expression types like `BoolExpr`, `MathExpr`, `CompExpr`, `NotExpr`, `LiteralExpr`, and more.
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
- `evalExpr`: The main evaluator function.
- `parseAndEval`: Parses and then evaluates a string.
- `evalString`: Evaluates a MiniRacket expression from a string.

### `MiniRacketParserSpec.hs`
Contains test cases for the MiniRacket parsers using `Hspec`

### `EvalSpec.hs`
Contains test cases for the evaluator functions for the MiniRacket AST using `Hspec`