module Language.Lambda.AST where

type Ident = String

data Term
  = Var Ident
  | Abst Term Term
  | App Term Term
  deriving (Show, Eq, Ord)