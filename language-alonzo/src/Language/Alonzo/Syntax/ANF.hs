module Language.Alonzo.Syntax.ANF where


data Exp var
  = Val Val
  | App Val Val
  | Let var Term Term

data Val var
  = Abs var Term
  | Var var