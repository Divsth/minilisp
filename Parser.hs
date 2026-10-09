module Parser (tokenize, parse, parseExpr) where

import Syntax
import Data.Char (isSpace)
import Text.Read (readMaybe)

-- Tokenizer
tokenize :: String -> [String]
tokenize [] = []
tokenize (c:cs)
  | isSpace c = tokenize cs
  | c == '('  = "(" : tokenize cs
  | c == ')'  = ")" : tokenize cs
  | c == '\'' = "'" : tokenize cs
  | c == '"'  =
      let (str, rest) = span (/= '"') cs
      in ('"' : str ++ "\"") : tokenize (drop 1 rest)
  | otherwise =
      let (tok, rest) = span (\x -> not (isSpace x || x == '(' || x == ')' || x == '\'')) (c:cs)
      in tok : tokenize rest

-- Atom Parser
parseAtom :: String -> Expr
parseAtom "#t" = LitBool True
parseAtom "#f" = LitBool False
parseAtom ('"':cs) = LitString (init cs)
parseAtom s = case readMaybe s of
  Just n  -> LitInt n
  Nothing -> Var s

-- Expression Parser
parseExpr :: [String] -> Either String (Expr, [String])
parseExpr []         = Left "Error: Unexpected end of input"
parseExpr ("'":ts)   = do
  (expr, rest) <- parseExpr ts
  Right (List [Var "quote", expr], rest)
parseExpr ("(":ts)   = parseList ts
parseExpr (")":_)    = Left "Error: Unexpected closing parenthesis ')'"
parseExpr (t:ts)     = Right (parseAtom t, ts)

-- List Parser
parseList :: [String] -> Either String (Expr, [String])
parseList []       = Left "Error: Missing closing parenthesis ')'"
parseList (")":ts) = Right (List [], ts)
parseList ts = do
  (expr, rest1) <- parseExpr ts
  res           <- parseList rest1
  case res of
    (List exprs, rest2) -> Right (List (expr : exprs), rest2)
    _                   -> Left "Error: Expected a list"

-- Entry Point
parse :: String -> Either String Expr
parse input = case parseExpr (tokenize input) of
  Left err -> Left err
  Right (ast, []) -> Right ast
  Right (_, remaining) -> Left $ "Error: Extra tokens after expression: " ++ unwords remaining