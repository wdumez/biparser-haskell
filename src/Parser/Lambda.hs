{-# OPTIONS_GHC -Wno-unused-imports #-}

module Parser.Lambda where

import Control.Applicative (Alternative (many, some, (<|>)))
import Data.Char (isLower, isSpace)
import Language.Lambda.AST
import Parser (P, between, char, eof, runParser', satisfy)

term :: P Term
term = var <|> abst <|> app

var :: P Term
var = Var <$> identifier

abst :: P Term
abst = parens (Abst <$> (backslash *> var <* dot) <*> term)

app :: P Term
app = parens (App <$> term <*> term)

identifier :: P Ident
identifier = token (some (satisfy isLower))

parens :: P a -> P a
parens = between openParens closeParens

openParens :: P Char
openParens = token (char '(')

closeParens :: P Char
closeParens = token (char ')')

backslash :: P Char
backslash = token (char '\\')

dot :: P Char
dot = token (char '.')

whitespace :: P String
whitespace = many (satisfy isSpace)

token :: P a -> P a
token p = p <* whitespace

fully :: P a -> P a
fully p = whitespace *> p <* eof

-- >>> runParser' (fully term) "( \\ x . ( \\ y . (x y) ) )"
-- Right (Abst (Var "x") (Abst (Var "y") (App (Var "x") (Var "y"))),("",[],Ln 1, Col 26))

-- >>> runParser' identifier "var"
-- Right ("var",("",[],Ln 1, Col 4))

-- >>> runParser' ((,) <$> identifier <*> identifier) "x y"
-- Right (("x","y"),("",[],Ln 1, Col 4))
