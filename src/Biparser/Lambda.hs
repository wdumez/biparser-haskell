{-# LANGUAGE LambdaCase #-}

module Biparser.Lambda where

import Biparser
import Control.Lens
import Data.Char (isLower)
import Language.Lambda.AST

data Cterm
  = Cvar
  | Cabst Cterm
  | Capp (Cterm, Cterm)
  deriving (Show, Eq, Ord)

wrapCterm :: Either (Either () Cterm) (Cterm, Cterm) -> Cterm
wrapCterm (Left (Left ())) = Cvar
wrapCterm (Left (Right c)) = Cabst c
wrapCterm (Right c) = Capp c

unwrapCterm :: Cterm -> Either (Either () Cterm) (Cterm, Cterm)
unwrapCterm Cvar = Left (Left ())
unwrapCterm (Cabst c) = Left (Right c)
unwrapCterm (Capp c) = Right c

term :: Biparser Cterm () Term
term =
  mapC wrapCterm unwrapCterm $
    var </> abst </> app

var :: Biparser () () Term
var = identifier >>#> mkVar

abst :: Biparser Cterm () Term
abst =
  parens $
    lambda
      #*>> var
      <<*# dot
      <<#*>> term
      >>#> mkAbst

app :: Biparser (Cterm, Cterm) () Term
app =
  parens $
    term
      <<*# space
      <<*>> term
      >>#> mkApp

parens :: Biparser c x y -> Biparser c x y
parens p = char '(' #*>> p <<*# char ')'

lambda :: Biparser () () ()
lambda = char '\\'

dot :: Biparser () () ()
dot = char '.'

space :: Biparser () () ()
space = char ' '

identifier :: Biparser () () Ident
identifier = some' (satisfy isLower)

mkVar :: Biparser () Ident Term
mkVar = mk "Var" (prism' Var (\case Var i -> Just i; _ -> Nothing))

mkAbst :: Biparser () (Term, Term) Term
mkAbst = mk "Abst" (prism' (uncurry Abst) (\case Abst t1 t2 -> Just (t1, t2); _ -> Nothing))

mkApp :: Biparser () (Term, Term) Term
mkApp = mk "App" (prism' (uncurry App) (\case App v t -> Just (v, t); _ -> Nothing))

-- >>> testParse term () "(\\x.(\\y.(x y)))"
-- Right ((Abst (Var "x") (Abst (Var "y") (App (Var "x") (Var "y"))),Cabst (Cabst (Capp (Cvar,Cvar)))),("",[],Ln 1, Col 16))

-- >>> testPrint term (Abst (Var "x") (Abst (Var "y") (App (Var "x") (Var "y"))), Nothing)
-- Right ((),("(\\x.(\\y.(x y)))",[],Ln 1, Col 16))
