module Syntax where

import qualified Data.Map as Map

-- Abstract Syntax Tree
data Expr
  = LitInt Integer
  | LitBool Bool
  | LitString String
  | Var String
  | List [Expr]
  deriving (Show, Eq)

-- Runtime Values
data Val
  = VInt Integer
  | VBool Bool
  | VString String
  | VList [Val]
  | VPrim ([Val] -> Either String Val)
  | VClosure [String] Expr Env

instance Show Val where
  show (VInt n)              = show n
  show (VBool True)          = "#t"
  show (VBool False)         = "#f"
  show (VString s)           = "\"" ++ s ++ "\""
  show (VList xs)            = "(" ++ unwords (map show xs) ++ ")"
  show (VPrim _)             = "<primitive>"
  show (VClosure params _ _) = "<closure (" ++ unwords params ++ ")>"

instance Eq Val where
  (VInt a)    == (VInt b)    = a == b
  (VBool a)   == (VBool b)   = a == b
  (VString a) == (VString b) = a == b
  (VList a)   == (VList b)   = a == b
  _           == _           = False

type Env = Map.Map String Val