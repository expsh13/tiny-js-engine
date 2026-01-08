module Lexer where

import Data.Char (isAlpha, isAlphaNum, isDigit, isSpace)
import Types (EvalError (..), Token (..))

-- Input:  "3 + (5 * 2)"
-- Output: [TokNumber 3, TokPlus, TokLParen, TokNumber 5, TokTimes, TokNumber 2, TokRParen, TokEOF]

tokenize :: String -> Either EvalError [Token]
tokenize [] = Right [TokEOF]
tokenize all@(x : xs)
  | isSpace x = tokenize xs
  | isDigit x =
      let (digitStr, rest) = span (\c -> isDigit c || c == '.') all
          num = read digitStr :: Double
       in fmap (TokNumber num :) (tokenize rest)
  | isAlpha x = tokenizeKeyword all
  | x == '"' = tokenizeString xs
  | x == '(' = fmap (TokLParen :) (tokenize xs)
  | x == ')' = fmap (TokRParen :) (tokenize xs)
  | x == '+' = fmap (TokPlus :) (tokenize xs)
  | x == '-' = fmap (TokMinus :) (tokenize xs)
  | x == '*' = fmap (TokTimes :) (tokenize xs)
  | x == '/' = fmap (TokDivide :) (tokenize xs)
  | x == '%' = fmap (TokModulo :) (tokenize xs)
  | x == '=' = case xs of
      ('=' : rest) -> fmap (TokEq :) (tokenize rest)
      _ -> Left (ParseError "Unexpected character: =")
  | x == '!' = case xs of
      ('=' : rest) -> fmap (TokNeq :) (tokenize rest)
      _ -> fmap (TokNot :) (tokenize xs)
  | x == '<' = case xs of
      ('=' : rest) -> fmap (TokLte :) (tokenize rest)
      _ -> fmap (TokLt :) (tokenize xs)
  | x == '>' = case xs of
      ('=' : rest) -> fmap (TokGte :) (tokenize rest)
      _ -> fmap (TokGt :) (tokenize xs)
  | x == '&' = case xs of
      ('&' : rest) -> fmap (TokAnd :) (tokenize rest)
      _ -> Left (ParseError "Unexpected character: &")
  | x == '|' = case xs of
      ('|' : rest) -> fmap (TokOr :) (tokenize rest)
      _ -> Left (ParseError "Unexpected character: |")
  | otherwise = Left (ParseError ("Unexpected character: " ++ [x]))

-- Parse keywords (true, false) or return error for unknown identifiers
tokenizeKeyword :: String -> Either EvalError [Token]
tokenizeKeyword input =
  let (word, rest) = span isAlphaNum input
   in case word of
        "true" -> fmap (TokTrue :) (tokenize rest)
        "false" -> fmap (TokFalse :) (tokenize rest)
        _ -> Left (ParseError ("Unknown identifier: " ++ word))

-- Parse string literals enclosed in double quotes
tokenizeString :: String -> Either EvalError [Token]
tokenizeString input =
  case break (== '"') input of
    (_, "") -> Left (ParseError "Unterminated string literal")
    (str, '"' : rest) -> fmap (TokString str :) (tokenize rest)
    _ -> Left (ParseError "Invalid string literal")
