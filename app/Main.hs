module Main where

import Evaluator (eval)
import Lexer (tokenize)
import Parser (parse)
import System.Console.Haskeline
import Types (Value (..))

-- REPL: Read-Eval-Print Loop
-- Provides an interactive shell for evaluating JavaScript expressions

main :: IO ()
main = runInputT defaultSettings $ do
  outputStrLn "╔═══════════════════════════════════════════╗"
  outputStrLn "║  Welcome to Tiny JS Engine!              ║"
  outputStrLn "║  Type expressions to evaluate them.       ║"
  outputStrLn "║  Commands: :quit or :q to exit            ║"
  outputStrLn "╚═══════════════════════════════════════════╝"
  outputStrLn ""
  repl

-- The main REPL loop
repl :: InputT IO ()
repl = do
  minput <- getInputLine "> "
  case minput of
    Nothing -> return () -- EOF (Ctrl+D)
    Just input -> do
      let trimmed = trim input
      if null trimmed
        then repl -- Empty input, continue
        else
          if isCommand trimmed
            then handleCommand trimmed
            else do
              evaluateExpression trimmed
              repl

-- Check if input is a command
isCommand :: String -> Bool
isCommand s = case s of
  ':' : _ -> True
  _ -> False

-- Handle REPL commands
handleCommand :: String -> InputT IO ()
handleCommand cmd = case cmd of
  ":quit" -> outputStrLn "Goodbye!"
  ":q" -> outputStrLn "Goodbye!"
  ":help" -> do
    outputStrLn "Available commands:"
    outputStrLn "  :quit, :q  - Exit the REPL"
    outputStrLn "  :help      - Show this help message"
    outputStrLn ""
    outputStrLn "Examples:"
    outputStrLn "  > 3 + 5 * 2"
    outputStrLn "  > (10 - 3) >= 5"
    outputStrLn "  > true && !false"
    outputStrLn "  > \"hello\" + \" world\""
    repl
  _ -> do
    outputStrLn $ "Unknown command: " ++ cmd
    outputStrLn "Type :help for available commands"
    repl

-- Evaluate an expression through the full pipeline
evaluateExpression :: String -> InputT IO ()
evaluateExpression input = do
  case tokenize input >>= parse >>= eval of
    Left err -> outputStrLn $ "Error: " ++ show err
    Right value -> outputStrLn $ formatValue value

-- Format a value for display
formatValue :: Value -> String
formatValue (VNumber n)
  | isInteger n = show (round n :: Integer)
  | otherwise = show n
  where
    isInteger x = x == fromIntegral (round x :: Integer)
formatValue (VString s) = show s
formatValue (VBool b) = if b then "true" else "false"

-- Trim whitespace from both ends of a string
trim :: String -> String
trim = f . f
  where
    f = reverse . dropWhile isSpace
    isSpace c = c `elem` " \t\n\r"
