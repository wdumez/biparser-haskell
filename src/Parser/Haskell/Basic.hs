{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Parser.Haskell.Basic where

import Control.Applicative
import Control.Monad
import qualified Data.Char as Char
import Parser
import Utils hiding (isSpace)
import Prelude hiding (exponent)

{- These are basic parsers, acting as building blocks. They provide no trailing whitespace consumption. -}

-- | @token p@ parses @p@, followed by whitespace which obeys line-folding rules.
token :: P a -> P a
token p = p <* lineFold

-- | @fully p@ parses @p@, requiring @p@ to consume all input.
fully :: P a -> P a
fully p = do
  x <- p
  void manyWhitespace
  eof
  pure x

special :: P Char
special = satisfy isSpecial <?> "special"

isSpecial :: Char -> Bool
isSpecial = (`elem` "(),;[]`{}")

space :: P Char
space = satisfy isSpace <?> "space"

isSpace :: Char -> Bool
isSpace = (== ' ')

dashes :: P String
dashes = munch' munchDashes isDashes <?> "dashes"

munchDashes :: Char -> Bool
munchDashes = (== '-')

isDashes :: String -> Bool
isDashes s = all (== '-') s && length s >= 2

graphic :: P Char
graphic = satisfy isGraphic <?> "graphic"

isGraphic :: Char -> Bool
isGraphic = any' [isSmall, isLarge, isSymbol, isDigit, isSpecial, (== '\"'), (== '\'')]

small :: P Char
small = satisfy isSmall <?> "small"

isSmall :: Char -> Bool
isSmall c = Char.isLower c || c == '_'

large :: P Char
large = satisfy isLarge <?> "large"

isLarge :: Char -> Bool
isLarge = Char.isUpper

symbol :: P Char
symbol = satisfy isSymbol <?> "symbol"

isSymbol :: Char -> Bool
isSymbol = (`elem` "!#$%&*+./<=>?@\\^|-~:")

digit :: P Char
digit = satisfy isDigit <?> "digit"

isDigit :: Char -> Bool
isDigit = Char.isDigit

octit :: P Char
octit = satisfy isOctit <?> "octit"

isOctit :: Char -> Bool
isOctit = Char.isOctDigit

hexit :: P Char
hexit = satisfy isHexit <?> "hexit"

isHexit :: Char -> Bool
isHexit = Char.isHexDigit

reservedid :: P String
reservedid = munch' munchReservedid isReservedid <?> "reservedid"

-- >>> runParser' reservedid "case"
-- >>> runParser' reservedid "cases"
-- Right ("case",("",[],Ln 1, Col 5))
-- Left invalid result

munchReservedid :: Char -> Bool
munchReservedid = (`elem` "_caseldtfurivngompxwyh")

isReservedid :: String -> Bool
isReservedid = flip elem ["_", "case", "class", "data", "default", "deriving", "do", "else", "foreign", "if", "import", "in", "infix", "infixl", "infixr", "instance", "let", "module", "newtype", "of", "then", "type", "where"]

reservedop :: P String
reservedop = munch' munchReservedop isReservedop <?> "reservedop"

munchReservedop :: Char -> Bool
munchReservedop = (`elem` ".:=>\\|<-@~")

isReservedop :: String -> Bool
isReservedop = flip elem ["..", "::", ":", "=>", "=", "\\", "|", "<-", "->", "@", "~"]

decimal :: P String
decimal = some digit <?> "decimal"

isDecimal :: String -> Bool
isDecimal = all isDigit

octal :: P String
octal = some octit <?> "octal"

isOctal :: String -> Bool
isOctal = all isOctit

hexadecimal :: P String
hexadecimal = some hexit <?> "hexadecimal"

isHexadecimal :: String -> Bool
isHexadecimal = all isHexit

exponent :: P String
exponent =
  do
    e <- string "e" <|> string "E"
    pm <- opt (string "+" <|> string "-")
    d <- decimal
    pure (e <> pm <> d)
    <?> "exponent"

isExponent :: String -> Bool
isExponent (c1 : c2 : cs) =
  c1 `elem` "eE"
    && (c2 `elem` "+-" || isDigit c2)
    && isDecimal cs
isExponent _ = False

varid :: P String
varid = munch' munchVarid isVarid <?> "varid"

munchVarid :: Char -> Bool
munchVarid = any' [isSmall, isLarge, isDigit, (== '\'')]

isVarid :: String -> Bool
isVarid s@(c : cs) =
  not (isReservedid s)
    && isSmall c
    && all munchVarid cs
isVarid _ = False

varsym :: P String
varsym = munch' munchVarsym isVarsym <?> "varsym"

-- >>> runParser' varsym "."
-- >>> runParser' varsym ".."
-- >>> runParser' varsym "..."
-- Right (".",("",[],Ln 1, Col 2))
-- Left invalid result
-- Right ("...",("",[],Ln 1, Col 4))

munchVarsym :: Char -> Bool
munchVarsym = isSymbol

isVarsym :: String -> Bool
isVarsym s@(c : _) =
  not (isReservedop s)
    && not (isDashes s)
    && (c /= ':')
    && all isSymbol s
isVarsym _ = False

conid :: P String
conid = munch' munchConid isConid <?> "conid"

munchConid :: Char -> Bool
munchConid = munchVarid

isConid :: String -> Bool
isConid (c : cs) =
  isLarge c
    && all (any' [isSmall, isLarge, isDigit, (== '\'')]) cs
isConid _ = False

consym :: P String
consym = token $ munch' munchConsym isConsym <?> "consym"

munchConsym :: Char -> Bool
munchConsym = isSymbol

isConsym :: String -> Bool
isConsym s@(c : cs) =
  not (isReservedop s)
    && c == ':'
    && all isSymbol cs
isConsym _ = False

tyvar :: P String
tyvar = varid <?> "tyvar"

tycon :: P String
tycon = conid <?> "tycon"

tycls :: P String
tycls = conid <?> "tycls"

modid :: P String
modid = join . punctuate "." <$> sepBy1 conid (string ".") <?> "modid"

-- >>> runParser' modid "F."
-- Right ("F",(".",[],Ln 1, Col 2))

qual :: P String
qual = opt (modid <* char '.') <?> "qual"

qvarid :: P (String, String)
qvarid = (,) <$> qual <*> varid <?> "qvarid"

qconid :: P (String, String)
qconid = (,) <$> qual <*> conid <?> "qconid"

qtycon :: P (String, String)
qtycon = (,) <$> qual <*> tycon <?> "qtycon"

qtycls :: P (String, String)
qtycls = (,) <$> qual <*> tycls <?> "qtycls"

qvarsym :: P (String, String)
qvarsym = (,) <$> qual <*> varsym <?> "qvarsym"

-- >>> runParser' (fully qvarsym) "F."     -- FAIL
-- >>> runParser' (fully qvarsym) "F.."    -- PASS
-- >>> runParser' (fully qvarsym) "F..."   -- FAIL
-- >>> runParser' (fully qvarsym) "F...."  -- PASS
-- Left invalid result
-- Right (("F","."),("",[],Ln 1, Col 4))
-- Left invalid result
-- Right (("F","..."),("",[],Ln 1, Col 6))

qconsym :: P (String, String)
qconsym = (,) <$> qual <*> consym <?> "qconsym"

hCons :: P String
hCons = munch' munchCons isCons <?> "hCons"

munchCons :: Char -> Bool
munchCons = isSymbol

isCons :: String -> Bool
isCons [':'] = True
isCons _ = False

float :: P String
float =
  choice
    [ decimal <<>> string "." <<>> decimal <<>> opt exponent,
      decimal <<>> exponent
    ]
    <?> "float"

integer :: P String
integer =
  choice
    [ decimal,
      string "0o" <<>> octal,
      string "0O" <<>> octal,
      string "0x" <<>> hexadecimal,
      string "0X" <<>> hexadecimal
    ]
    <?> "integer"

hChar :: P Char
hChar =
  do
    _ <- char '\''
    c <- choice $ map satisfy [\c -> isGraphic c && c /= '\'' && c /= '\\', isSpace]
    _ <- char '\''
    pure c
    <?> "hChar"

hString :: P String
hString =
  do
    _ <- char '\"'
    s <- many $ choice $ map satisfy [\c -> isGraphic c && c /= '\"' && c /= '\\', isSpace]
    _ <- char '\"'
    pure s
    <?> "hString"
