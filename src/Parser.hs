module Parser where

import Types (BinOperator (..), EvalError (..), Expr (..), Token (..), UnaryOperator (..))

-- Pratt Parsing (Operator Precedence Parsing)
--
-- Input:  [TokNumber 3, TokPlus, TokNumber 5, TokTimes, TokNumber 2, TokEOF]
-- Output: BinOp Plus (NumLit 3) (BinOp Times (NumLit 5) (NumLit 2))
--
-- The parser builds an Abstract Syntax Tree (AST) that represents operator precedence.
-- Higher precedence operators bind more tightly and appear deeper in the tree.
--
-- Example: "3 + 5 * 2"
--      +           (Plus has lower precedence, so it's at the root)
--     / \
--    3   *         (Times has higher precedence, so it's deeper)
--       / \
--      5   2

-- Main parse function: entry point
parse :: [Token] -> Either EvalError Expr
parse tokens = case parseExpr tokens of
  Right (expr, [TokEOF]) -> Right expr
  Right (_, remaining) -> Left (ParseError ("Unexpected tokens remaining: " ++ show remaining))
  Left err -> Left err

-- Parse an expression and return the remaining tokens
parseExpr :: [Token] -> Either EvalError (Expr, [Token])
parseExpr tokens = parseExprWithPrecedence tokens 0

-- Operator precedence table (lower number = lower precedence)
-- Operators with same precedence are handled left-to-right (left-associative)
precedence :: Token -> Int
precedence TokOr = 1
precedence TokAnd = 2
precedence TokEq = 3
precedence TokNeq = 3
precedence TokLt = 4
precedence TokGt = 4
precedence TokLte = 4
precedence TokGte = 4
precedence TokPlus = 5
precedence TokMinus = 5
precedence TokTimes = 6
precedence TokDivide = 6
precedence TokModulo = 6
precedence _ = 0 -- Non-operators have precedence 0

-- Convert Token to BinOperator
tokenToBinOp :: Token -> Maybe BinOperator
tokenToBinOp TokPlus = Just Plus
tokenToBinOp TokMinus = Just Minus
tokenToBinOp TokTimes = Just Times
tokenToBinOp TokDivide = Just Divide
tokenToBinOp TokModulo = Just Modulo
tokenToBinOp TokEq = Just Eq
tokenToBinOp TokNeq = Just Neq
tokenToBinOp TokLt = Just Lt
tokenToBinOp TokGt = Just Gt
tokenToBinOp TokLte = Just Lte
tokenToBinOp TokGte = Just Gte
tokenToBinOp TokAnd = Just And
tokenToBinOp TokOr = Just Or
tokenToBinOp _ = Nothing

-- Parse primary expressions: literals, unary operators, and parenthesized expressions
parsePrimary :: [Token] -> Either EvalError (Expr, [Token])
parsePrimary [] = Left (ParseError "Unexpected end of input")
parsePrimary (tok : rest) = case tok of
  TokNumber n -> Right (NumLit n, rest)
  TokString s -> Right (StrLit s, rest)
  TokTrue -> Right (BoolLit True, rest)
  TokFalse -> Right (BoolLit False, rest)
  TokNot -> do
    (expr, remaining) <- parseExprWithPrecedence rest 7 -- Unary precedence is 7
    Right (UnaryOp Not expr, remaining)
  TokMinus -> do
    (expr, remaining) <- parseExprWithPrecedence rest 7 -- Unary precedence is 7
    Right (UnaryOp Negate expr, remaining)
  TokLParen -> do
    (expr, remaining) <- parseExpr rest
    case remaining of
      (TokRParen : rest') -> Right (expr, rest')
      _ -> Left (ParseError "Expected closing parenthesis")
  _ -> Left (ParseError ("Unexpected token: " ++ show tok))

-- Core Pratt parsing algorithm
-- This is the heart of the parser - it handles operator precedence and left-associativity
parseExprWithPrecedence :: [Token] -> Int -> Either EvalError (Expr, [Token])
parseExprWithPrecedence tokens minPrec = do
  -- Step 1: Parse the first operand (a primary expression)
  (left, rest) <- parsePrimary tokens

  -- Step 2: Loop through binary operators
  -- While the next token is an operator with precedence >= minPrec:
  --   1. Get the operator and its precedence
  --   2. Recursively parse the right operand with higher precedence
  --   3. Combine left and right into a BinOp node
  --   4. Continue with the new expression as the left operand
  parseInfix left rest minPrec

-- Infix operator parsing loop
-- Handles binary operators with precedence >= minPrec
parseInfix :: Expr -> [Token] -> Int -> Either EvalError (Expr, [Token])
parseInfix left [] _ = Right (left, [])
parseInfix left rest@(tok : tokens) minPrec =
  case tokenToBinOp tok of
    Nothing -> Right (left, rest) -- Not a binary operator, stop
    Just op ->
      let prec = precedence tok
       in if prec < minPrec
            then Right (left, rest) -- Precedence too low, stop
            else do
              -- Parse right operand with higher precedence (prec + 1 for left-associativity)
              (right, remaining) <- parseExprWithPrecedence tokens (prec + 1)
              -- Combine into BinOp and continue parsing
              let newExpr = BinOp op left right
              parseInfix newExpr remaining minPrec
