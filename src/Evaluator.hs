module Evaluator (eval) where

import Types (BinOperator (..), EvalError (..), Expr (..), UnaryOperator (..), Value (..))

-- Evaluator: Transforms AST into final values
--
-- Input:  BinOp Plus (NumLit 3) (NumLit 5)
-- Output: VNumber 8.0
--
-- The evaluator recursively walks the AST and computes the final result.
-- It enforces strict typing - no JavaScript-style type coercion.

eval :: Expr -> Either EvalError Value
eval (NumLit n) = Right (VNumber n)
eval (StrLit s) = Right (VString s)
eval (BoolLit b) = Right (VBool b)
eval (UnaryOp op expr) = do
  val <- eval expr
  evalUnaryOp op val
eval (BinOp op left right) = do
  leftVal <- eval left
  rightVal <- eval right
  evalBinOp op leftVal rightVal

-- Evaluate unary operators
evalUnaryOp :: UnaryOperator -> Value -> Either EvalError Value
evalUnaryOp Not (VBool b) = Right (VBool (not b))
evalUnaryOp Not _ = Left (TypeError "Logical NOT requires boolean operand")
evalUnaryOp Negate (VNumber n) = Right (VNumber (-n))
evalUnaryOp Negate _ = Left (TypeError "Negation requires numeric operand")

-- Evaluate binary operators
evalBinOp :: BinOperator -> Value -> Value -> Either EvalError Value
-- Arithmetic operators (numbers only)
evalBinOp Plus (VNumber a) (VNumber b) = Right (VNumber (a + b))
evalBinOp Plus (VString a) (VString b) = Right (VString (a ++ b)) -- String concatenation
evalBinOp Plus _ _ = Left (TypeError "Addition requires two numbers or two strings")
evalBinOp Minus (VNumber a) (VNumber b) = Right (VNumber (a - b))
evalBinOp Minus _ _ = Left (TypeError "Subtraction requires numeric operands")
evalBinOp Times (VNumber a) (VNumber b) = Right (VNumber (a * b))
evalBinOp Times _ _ = Left (TypeError "Multiplication requires numeric operands")
evalBinOp Divide (VNumber _) (VNumber 0) = Left DivisionByZero
evalBinOp Divide (VNumber a) (VNumber b) = Right (VNumber (a / b))
evalBinOp Divide _ _ = Left (TypeError "Division requires numeric operands")
evalBinOp Modulo (VNumber _) (VNumber 0) = Left DivisionByZero
evalBinOp Modulo (VNumber a) (VNumber b) = Right (VNumber (mod' a b))
  where
    mod' x y = x - fromIntegral (floor (x / y)) * y
evalBinOp Modulo _ _ = Left (TypeError "Modulo requires numeric operands")
-- Comparison operators (same types only)
evalBinOp Eq (VNumber a) (VNumber b) = Right (VBool (a == b))
evalBinOp Eq (VString a) (VString b) = Right (VBool (a == b))
evalBinOp Eq (VBool a) (VBool b) = Right (VBool (a == b))
evalBinOp Eq _ _ = Left (TypeError "Equality comparison requires same types")
evalBinOp Neq (VNumber a) (VNumber b) = Right (VBool (a /= b))
evalBinOp Neq (VString a) (VString b) = Right (VBool (a /= b))
evalBinOp Neq (VBool a) (VBool b) = Right (VBool (a /= b))
evalBinOp Neq _ _ = Left (TypeError "Inequality comparison requires same types")
evalBinOp Lt (VNumber a) (VNumber b) = Right (VBool (a < b))
evalBinOp Lt _ _ = Left (TypeError "Less-than comparison requires numeric operands")
evalBinOp Gt (VNumber a) (VNumber b) = Right (VBool (a > b))
evalBinOp Gt _ _ = Left (TypeError "Greater-than comparison requires numeric operands")
evalBinOp Lte (VNumber a) (VNumber b) = Right (VBool (a <= b))
evalBinOp Lte _ _ = Left (TypeError "Less-than-or-equal comparison requires numeric operands")
evalBinOp Gte (VNumber a) (VNumber b) = Right (VBool (a >= b))
evalBinOp Gte _ _ = Left (TypeError "Greater-than-or-equal comparison requires numeric operands")
-- Logical operators (booleans only)
evalBinOp And (VBool a) (VBool b) = Right (VBool (a && b))
evalBinOp And _ _ = Left (TypeError "Logical AND requires boolean operands")
evalBinOp Or (VBool a) (VBool b) = Right (VBool (a || b))
evalBinOp Or _ _ = Left (TypeError "Logical OR requires boolean operands")
