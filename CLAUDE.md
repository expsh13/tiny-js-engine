# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

A tiny JavaScript expression evaluator written in Haskell as a learning project. Implements a classic interpreter pipeline: Lexer → Parser → Evaluator → REPL.

**Scope**: Expression evaluation only (no statements, variables, or functions)
- Literals: numbers, strings, booleans
- Operators: arithmetic (`+`, `-`, `*`, `/`, `%`), comparison (`==`, `!=`, `<`, `>`, `<=`, `>=`), logical (`&&`, `||`, `!`)
- Parentheses for grouping

## Build Commands

```bash
# Build project
cabal build

# Run REPL
cabal run tiny-js-engine

# Run tests
cabal test

# Run specific test module
cabal test --test-option=--match --test-option="Lexer"

# Interactive development (load modules in GHCi)
cabal repl
```

## Architecture

**Data Flow Pipeline:**
```
String Input → Tokenize → Parse → Evaluate → Value
                  ↓          ↓        ↓
               [Token]     Expr    Value
```

**Core Modules (in dependency order):**

1. **Types.hs** - Foundation module defining all data types
   - `Token`: Lexer output (TokNumber, TokPlus, etc.)
   - `Expr`: AST nodes (NumLit, BinOp, UnaryOp)
   - `Value`: Runtime values (VNumber, VString, VBool)
   - `EvalError`: Error types (ParseError, TypeError, DivisionByZero)
   - All other modules depend on this

2. **Lexer.hs** - Tokenization
   - Main function: `tokenize :: String -> Either EvalError [Token]`
   - Converts input string to token stream
   - Handles: numbers, strings (quotes), booleans, operators, parentheses
   - Must handle two-character operators (`==`, `!=`, `<=`, `>=`, `&&`, `||`) before single-character ones

3. **Parser.hs** - AST Construction
   - Main function: `parse :: [Token] -> Either EvalError Expr`
   - Uses **Pratt parsing** (precedence climbing) algorithm
   - Operator precedence (lowest to highest):
     - `||` (1), `&&` (2), `==`/`!=` (3), `<`/`>`/`<=`/`>=` (4)
     - `+`/`-` (5), `*`/`/`/`%` (6), `!` unary (7)
   - Left-associative for all binary operators
   - This is typically the most complex module

4. **Evaluator.hs** - Expression Evaluation
   - Main function: `eval :: Expr -> Either EvalError Value`
   - Recursively evaluates AST to produce values
   - **Type system**: Strict typing (no JavaScript-like coercion)
     - Arithmetic ops: numbers only
     - Logical ops: booleans only
     - String concatenation: `+` on two strings
     - Type mismatches produce TypeError
   - Handles division by zero

5. **Main.hs** (in app/) - REPL
   - Uses `haskeline` library for input with history/editing
   - Composes: `tokenize >>= parse >>= eval`
   - Commands: `:quit` or `:q` to exit

## Error Handling Pattern

All functions use `Either EvalError a` for composability:
```haskell
tokenize :: String -> Either EvalError [Token]
parse :: [Token] -> Either EvalError Expr
eval :: Expr -> Either EvalError Value
```

Chain with `>>=` operator in REPL: `tokenize input >>= parse >>= eval`

## Type System Design

**Strict typing** (unlike JavaScript):
- No implicit coercion
- Clear type errors instead of surprising behavior
- Example: `"hello" + 5` → TypeError (not "hello5")
- String concatenation only works with two strings

## Parser Implementation Notes

Uses Pratt parsing for operator precedence. Key components:
- `parsePrimary`: Handles literals, unary operators, parentheses
- `parseInfix`: Handles binary operators with precedence checking
- `parseExprWithPrecedence`: Main recursive function with minimum precedence parameter

Precedence table should be centralized (e.g., `precedence :: Token -> Int`)

## Testing Strategy

- **LexerSpec.hs**: Test each token type individually
- **ParserSpec.hs**: Test operator precedence, associativity, error cases
- **EvaluatorSpec.hs**: Test all operations, type errors, edge cases (division by zero)
- Use HSpec for unit tests, QuickCheck for property-based testing

## Development Guidelines

- **Module order**: Always implement Types → Lexer → Parser → Evaluator → Main
- **Incremental testing**: Test each module thoroughly before moving to the next
- **Parser complexity**: Expect the parser to be the most challenging component
- **Type signatures**: Always include explicit type signatures for top-level functions
- **Error messages**: Make ParseError messages descriptive (include position/context when possible)

## Dependencies

- **base**: Standard library
- **mtl**: Monad transformers (StateT for lexer/parser)
- **haskeline**: REPL with readline support
- **hspec**: Testing framework
- **QuickCheck**: Property-based testing
