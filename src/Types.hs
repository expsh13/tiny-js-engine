-- Module      : Types
-- Description : Core data types for the tiny JavaScript expression evaluator
-- License     : MIT
--
-- This module defines all the fundamental types used throughout the interpreter:
-- - Token: Output from the lexer (tokenization phase)
-- - Expr: Abstract Syntax Tree nodes (parser output)
-- - Value: Runtime values (evaluator output)
-- - EvalError: All possible errors that can occur
--
-- These types form the "contract" between modules. Each phase transforms data:
--   String → [Token] → Expr → Value
--            (Lexer)  (Parser) (Evaluator)
module Types where

--------------------------------------------------------------------------------
-- 1. TOKEN TYPE
--------------------------------------------------------------------------------
{-
The Token type represents the smallest meaningful units from the input string.
The lexer breaks input like "3 + 5 * 2" into tokens: [TokNumber 3, TokPlus, TokNumber 5, TokTimes, TokNumber 2]

You need to define tokens for:

LITERALS:
  - TokNumber Double       -- Numbers: 42, 3.14, -5
  - TokString String       -- Strings: "hello", "world"
  - TokTrue                -- Boolean: true
  - TokFalse               -- Boolean: false

{-

ARITHMETIC OPERATORS:
  - TokPlus                -- +
  - TokMinus               -- -
  - TokTimes               -- *
  - TokDivide              -- /
  - TokModulo              -- %

COMPARISON OPERATORS (note: these are TWO characters each):
  - TokEq                  -- ==
  - TokNeq                 -- !=
  - TokLt                  -- <
  - TokGt                  -- >
  - TokLte                 -- <=
  - TokGte                 -- >=

LOGICAL OPERATORS:
  - TokAnd                 -- &&
  - TokOr                  -- ||
  - TokNot                 -- !

STRUCTURAL:
  - TokLParen              -- (
  - TokRParen              -- )
  - TokEOF                 -- End of input (helps parser know when to stop)

EXAMPLE USAGE:
  Input:  "3 + (5 * 2)"
  Tokens: [TokNumber 3, TokPlus, TokLParen, TokNumber 5, TokTimes, TokNumber 2, TokRParen, TokEOF]

NOTE: This should be an algebraic data type (using 'data' keyword)
NOTE: Derive Show and Eq for debugging and testing
-}

-- TODO(human): Define the Token data type here
-- data Token = ...

-}

data Token
  = TokNumber Double
  | TokString String
  | TokTrue
  | TokFalse
  | TokPlus
  | TokMinus
  | TokTimes
  | TokDivide
  | TokModulo
  | TokEq
  | TokNeq
  | TokLt
  | TokGt
  | TokLte
  | TokGte
  | TokAnd
  | TokOr
  | TokNot
  | TokLParen
  | TokRParen
  | TokEOF
  deriving (Show, Eq)

--------------------------------------------------------------------------------
-- 2. EXPR TYPE (Abstract Syntax Tree)
--------------------------------------------------------------------------------
{-
The Expr type represents the structure of expressions after parsing.
It's a tree where each node is an operation and leaves are literal values.

You need to define:

LITERALS:
  - NumLit Double          -- Number literals: NumLit 42
  - StrLit String          -- String literals: StrLit "hello"
  - BoolLit Bool           -- Boolean literals: BoolLit True

BINARY OPERATIONS (operations with two operands):
  - BinOp BinOperator Expr Expr
    Where BinOperator is another type defining: Plus, Minus, Times, Divide, Modulo,
    Eq, Neq, Lt, Gt, Lte, Gte, And, Or

UNARY OPERATIONS (operations with one operand):
  - UnaryOp UnaryOperator Expr
    Where UnaryOperator is another type defining: Not, Negate

EXAMPLE TREE:
  Input:      "3 + 5 * 2"
  AST:        BinOp Plus (NumLit 3) (BinOp Times (NumLit 5) (NumLit 2))

  Tree visualization:
         +
        / \
       3   *
          / \
         5   2

  Input:      "!(x == 5)"
  AST:        UnaryOp Not (BinOp Eq (StrLit "x") (NumLit 5))

NOTE: You'll need to define THREE types:
  1. data BinOperator = Plus | Minus | Times | ...
  2. data UnaryOperator = Not | Negate
  3. data Expr = NumLit Double | StrLit String | BinOp BinOperator Expr Expr | ...

NOTE: Derive Show and Eq for all three types
-}

-- TODO(human): Define the BinOperator data type here
data BinOperator = Plus | Minus | Times | Divide | Modulo | Eq | Neq | Lt | Gt | Lte | Gte | And | Or
  deriving (Show, Eq)

-- TODO(human): Define the UnaryOperator data type here
data UnaryOperator = Not | Negate
  deriving (Show, Eq)

-- TODO(human): Define the Expr data type here
data Expr
  = NumLit Double
  | StrLit String
  | BoolLit Bool
  | BinOp BinOperator Expr Expr
  | UnaryOp UnaryOperator Expr
  deriving (Show, Eq)

--------------------------------------------------------------------------------
-- 3. VALUE TYPE (Runtime Values)
--------------------------------------------------------------------------------
{-
The Value type represents the result of evaluating an expression.
After the evaluator processes an Expr tree, it produces a Value.

You need to define:
  - VNumber Double         -- Numeric result: VNumber 42.0
  - VString String         -- String result: VString "hello world"
  - VBool Bool            -- Boolean result: VBool True

EXAMPLE EVALUATION:
  Expression: BinOp Plus (NumLit 3) (NumLit 5)
  Evaluates to: VNumber 8.0

  Expression: BinOp Eq (NumLit 3) (NumLit 3)
  Evaluates to: VBool True

  Expression: BinOp Plus (StrLit "hello") (StrLit " world")
  Evaluates to: VString "hello world"

NOTE: This is simpler than Expr - it's just the three value types
NOTE: Derive Show and Eq
-}

-- TODO(human): Define the Value data type here
data Value = VNumber Double | VString String | VBool Bool
  deriving (Show, Eq)

--------------------------------------------------------------------------------
-- 4. EVALERROR TYPE (Error Handling)
--------------------------------------------------------------------------------
{-
The EvalError type represents all possible errors that can occur during
tokenization, parsing, or evaluation.

You need to define:
  - ParseError String      -- Parsing failed: ParseError "Unexpected token: )"
  - TypeError String       -- Type mismatch: TypeError "Cannot add number and string"
  - DivisionByZero         -- Tried to divide by zero: DivisionByZero

EXAMPLE ERRORS:
  Input: "3 + + 5"
  Error: ParseError "Unexpected token: TokPlus"

  Input: "3 + true"
  Error: TypeError "Cannot perform arithmetic on boolean"

  Input: "5 / 0"
  Error: DivisionByZero

NOTE: All functions in the pipeline return Either EvalError <result>
  - Lexer:     tokenize :: String -> Either EvalError [Token]
  - Parser:    parse :: [Token] -> Either EvalError Expr
  - Evaluator: eval :: Expr -> Either EvalError Value

NOTE: Derive Show and Eq
-}

-- TODO(human): Define the EvalError data type here
data EvalError = ParseError String | TypeError String | DivisionByZero deriving (Show, Eq)

--------------------------------------------------------------------------------
-- SUMMARY
--------------------------------------------------------------------------------
{-
Once you implement these four types, the module dependencies will be:

  Types.hs (this file - no dependencies)
     ↓
  Lexer.hs (uses Token, EvalError)
     ↓
  Parser.hs (uses Token, Expr, BinOperator, UnaryOperator, EvalError)
     ↓
  Evaluator.hs (uses Expr, Value, EvalError)
     ↓
  Main.hs (uses all types, composes: tokenize >>= parse >>= eval)

Each TODO(human) above marks where you need to write the data type definition.
Remember to derive Show and Eq for all types!
-}
