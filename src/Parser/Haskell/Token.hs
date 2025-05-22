module Parser.Haskell.Token where

import Parser
import Parser.Haskell.Basic
import Prelude hiding (exponent)

{- These are token parsers, meaning they correctly consume trailing whitespace with line-folding behavior. -}

keyword :: String -> P String
keyword = token . string

colon :: P String
colon = keyword ":" <?> "colon"

doubleColon :: P String
doubleColon = keyword "::" <?> "doubleColon"

rightArrow :: P String
rightArrow = keyword "->" <?> "rightArrow"

bigRightArrow :: P String
bigRightArrow = keyword "=>" <?> "bigRightArrow"

backslash :: P String
backslash = keyword "\\" <?> "backslash"

openParen :: P String
openParen = keyword "(" <?> "openParen"

closeParen :: P String
closeParen = keyword ")" <?> "closeParen"

openBracket :: P String
openBracket = keyword "[" <?> "openBracket"

closeBracket :: P String
closeBracket = keyword "]" <?> "closeBracket"

backTick :: P String
backTick = keyword "`" <?> "backTick"

comma :: P String
comma = keyword "," <?> "comma"

underscore :: P String
underscore = keyword "_" <?> "underscore"

pipe :: P String
pipe = keyword "|" <?> "pipe"

equals :: P String
equals = keyword "=" <?> "equals"

minus :: P String
minus = keyword "-" <?> "minus"

parens :: P a -> P a
parens p = between openParen closeParen p <?> "parens"

brackets :: P a -> P a
brackets p = between openBracket closeBracket p <?> "brackets"

backTicks :: P a -> P a
backTicks p = between backTick backTick p <?> "backTicks"

kwIf :: P String
kwIf = keyword "if" <?> "kwIf"

kwThen :: P String
kwThen = keyword "then" <?> "kwThen"

kwElse :: P String
kwElse = keyword "else" <?> "kwElse"

kwWhere :: P String
kwWhere = keyword "where" <?> "kwWhere"

kwLet :: P String
kwLet = keyword "let" <?> "kwLet"

kwIn :: P String
kwIn = keyword "in" <?> "kwIn"

kwCase :: P String
kwCase = keyword "case" <?> "kwCase"

kwOf :: P String
kwOf = keyword "of" <?> "kwOf"

kwInfixl :: P String
kwInfixl = keyword "infixl" <?> "kwInfixl"

kwInfixr :: P String
kwInfixr = keyword "infixr" <?> "kwInfixr"

kwInfix :: P String
kwInfix = keyword "infix" <?> "kwInfix"
