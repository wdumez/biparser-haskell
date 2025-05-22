module Biparser.Haskell.Token where

import Biparser
import Biparser.Haskell.Basic
import Biparser.Haskell.Complements
import Prelude hiding (exponent)

{- These are token parsers, meaning they correctly consume trailing whitespace with line-folding behavior. -}

keyword :: String -> Biparser CToken () ()
keyword = token' . string

colon :: Biparser CToken () ()
colon = keyword ":" <?> "colon"

doubleColon :: Biparser CToken () ()
doubleColon = keyword "::" <?> "doubleColon"

rightArrow :: Biparser CToken () ()
rightArrow = keyword "->" <?> "rightArrow"

bigRightArrow :: Biparser CToken () ()
bigRightArrow = keyword "=>" <?> "bigRightArrow"

backslash :: Biparser CToken () ()
backslash = keyword "\\" <?> "backslash"

openParen :: Biparser CToken () ()
openParen = keyword "(" <?> "openParen"

closeParen :: Biparser CToken () ()
closeParen = keyword ")" <?> "closeParen"

openBracket :: Biparser CToken () ()
openBracket = keyword "[" <?> "openBracket"

closeBracket :: Biparser CToken () ()
closeBracket = keyword "]" <?> "closeBracket"

backTick :: Biparser CToken () ()
backTick = keyword "`" <?> "backTick"

comma :: Biparser CToken () ()
comma = keyword "," <?> "comma"

underscore :: Biparser CToken () ()
underscore = keyword "_" <?> "underscore"

pipe :: Biparser CToken () ()
pipe = keyword "|" <?> "pipe"

equals :: Biparser CToken () ()
equals = keyword "=" <?> "equals"

minus :: Biparser CToken () ()
minus = keyword "-" <?> "minus"

parens :: Biparser c x y -> Biparser (CParens c) x y
parens p = between openParen closeParen p <?> "parens"

brackets :: Biparser c x y -> Biparser (CBrackets c) x y
brackets p = between openBracket closeBracket p <?> "brackets"

backTicks :: Biparser c x y -> Biparser (CBackTicks c) x y
backTicks p = between backTick backTick p <?> "backTicks"

kwIf :: Biparser CToken () ()
kwIf = keyword "if" <?> "kwIf"

kwThen :: Biparser CToken () ()
kwThen = keyword "then" <?> "kwThen"

kwElse :: Biparser CToken () ()
kwElse = keyword "else" <?> "kwElse"

kwWhere :: Biparser CToken () ()
kwWhere = keyword "where" <?> "kwWhere"

kwLet :: Biparser CToken () ()
kwLet = keyword "let" <?> "kwLet"

kwIn :: Biparser CToken () ()
kwIn = keyword "in" <?> "kwIn"

kwCase :: Biparser CToken () ()
kwCase = keyword "case" <?> "kwCase"

kwOf :: Biparser CToken () ()
kwOf = keyword "of" <?> "kwOf"

kwInfixl :: Biparser CToken () ()
kwInfixl = keyword "infixl" <?> "kwInfixl"

kwInfixr :: Biparser CToken () ()
kwInfixr = keyword "infixr" <?> "kwInfixr"

kwInfix :: Biparser CToken () ()
kwInfix = keyword "infix" <?> "kwInfix"
