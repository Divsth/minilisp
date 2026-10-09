module Eval (eval, defaultEnv) where

import Syntax
import qualified Data.Map as Map

-- Standard Environment
defaultEnv :: Env
defaultEnv = Map.fromList
  [ ("+",     VPrim (numericOp (+)))
  , ("-",     VPrim (numericOp (-)))
  , ("*",     VPrim (numericOp (*)))
  , ("=",     VPrim (comparisonOp (==)))
  , ("<",     VPrim (comparisonOp (<)))
  , (">",     VPrim (comparisonOp (>)))
  , ("cons",  VPrim primCons)
  , ("car",   VPrim primCar)
  , ("cdr",   VPrim primCdr)
  , ("null?", VPrim primNull)
  , ("list",  VPrim (Right . VList))
  ]

-- Arithmetic Helpers
numericOp :: (Integer -> Integer -> Integer) -> [Val] -> Either String Val
numericOp op [VInt a, VInt b] = Right (VInt (a `op` b))
numericOp _  _                = Left "Type error: expected two integers"

comparisonOp :: (Integer -> Integer -> Bool) -> [Val] -> Either String Val
comparisonOp op [VInt a, VInt b] = Right (VBool (a `op` b))
comparisonOp _  _                = Left "Type error: expected two integers for comparison"

-- List Primitives
primCons :: [Val] -> Either String Val
primCons [x, VList xs] = Right (VList (x : xs))
primCons [x, y]        = Right (VList [x, y])
primCons _             = Left "Arity error: cons expects 2 arguments"

primCar :: [Val] -> Either String Val
primCar [VList (x:_)] = Right x
primCar [VList []]    = Left "Runtime error: car called on empty list"
primCar _             = Left "Type error: car expects a list"

primCdr :: [Val] -> Either String Val
primCdr [VList (_:xs)] = Right (VList xs)
primCdr [VList []]     = Left "Runtime error: cdr called on empty list"
primCdr _             = Left "Type error: cdr expects a list"

primNull :: [Val] -> Either String Val
primNull [VList []] = Right (VBool True)
primNull [VList _]  = Right (VBool False)
primNull _          = Left "Type error: null? expects a list"

-- Core Evaluator
eval :: Expr -> Env -> Either String (Val, Env)

eval (LitInt n)    env = Right (VInt n, env)
eval (LitBool b)   env = Right (VBool b, env)
eval (LitString s) env = Right (VString s, env)

eval (Var name) env =
  case Map.lookup name env of
    Just v  -> Right (v, env)
    Nothing -> Left $ "Unbound variable: " ++ name

-- Quoted expressions
eval (List [Var "quote", expr]) env = Right (quoteToVal expr, env)
  where
    quoteToVal (LitInt n)    = VInt n
    quoteToVal (LitBool b)   = VBool b
    quoteToVal (LitString s) = VString s
    quoteToVal (Var s)       = VString s
    quoteToVal (List es)     = VList (map quoteToVal es)

-- Variable definition
eval (List [Var "define", Var name, expr]) env = do
  (val, _) <- eval expr env
  let newEnv = Map.insert name val env
  Right (val, newEnv)

-- Conditionals
eval (List [Var "if", cond, thenExpr, elseExpr]) env = do
  (cVal, _) <- eval cond env
  case cVal of
    VBool True  -> eval thenExpr env
    VBool False -> eval elseExpr env
    _           -> Left "Type error: condition must evaluate to a boolean (#t or #f)"

-- Closures
eval (List [Var "lambda", List params, body]) env = do
  paramNames <- mapM extractParam params
  Right (VClosure paramNames body env, env)
  where
    extractParam (Var p) = Right p
    extractParam _       = Left "Syntax error: lambda parameters must be symbols"

-- Function application
eval (List (fnExpr : argExprs)) env = do
  (fnVal, _) <- eval fnExpr env
  evaluatedArgs <- mapM (\arg -> fst <$> eval arg env) argExprs
  case fnVal of
    VPrim f -> do
      result <- f evaluatedArgs
      Right (result, env)

    VClosure params body capturedEnv ->
      if length params /= length evaluatedArgs
        then Left "Arity error: incorrect number of arguments passed to function"
        else do
          let callEnv = Map.unions [Map.fromList (zip params evaluatedArgs), capturedEnv, env]
          (res, _) <- eval body callEnv
          Right (res, env)

    _ -> Left "Evaluation error: first item in expression must be a function"

eval (List []) _ = Left "Syntax error: cannot evaluate empty list ()"