module Biparser.Haskell.Basic where

import Biparser
import Biparser.Haskell.Complements
import Control.Monad
import qualified Data.List as List
import PErr
import qualified Parser.Haskell.Basic as PB
import Utils
import Prelude hiding (exp, exponent, print)

{- These are basic parsers, acting as building blocks. They provide no trailing whitespace consumption. -}

singleton :: Biparser () x [x]
singleton = partialIso toSingle fromSingle <?> "singleton"
  where
    toSingle x = pure [x]
    fromSingle [x] = Right x
    fromSingle xs = Left $ Error $ "expected one element, got: " ++ show (length xs)

token :: Biparser c x y -> Biparser (c, CToken) x y
token p = p <<* lineFold

token' :: Biparser () x y -> Biparser CToken x y
token' = mapC unitL' unitL . token

-- IMPORTANT: we also discard whitespace at the end, because our token combinators will not always fully consume it (since it's important for line folding).
-- Also, we do *not* parse initial whitespace, because it is important to determine the indentation level. If you want to parse a non-block combinator, then don't use fully.
fully :: Biparser c x y -> Biparser (c, Whitespace) x y
fully p =
  p
    <<* (align allWhitespace $ manyWhitespace >#>> discard "")
    <<*# eof

special :: Biparser () () Char
special = satisfy PB.isSpecial

space :: Biparser () () Char
space = satisfy PB.isSpace

dashes :: Biparser () () String
dashes = munch' PB.munchDashes PB.isDashes

graphic :: Biparser () () Char
graphic = satisfy PB.isGraphic

small :: Biparser () () Char
small = satisfy PB.isSmall

large :: Biparser () () Char
large = satisfy PB.isLarge

symbol :: Biparser () () Char
symbol = satisfy PB.isSymbol

digit :: Biparser () () Char
digit = satisfy PB.isDigit

octit :: Biparser () () Char
octit = satisfy PB.isOctit

hexit :: Biparser () () Char
hexit = satisfy PB.isHexit

reservedid :: Biparser () () String
reservedid = munch' PB.munchReservedid PB.isReservedid

reservedop :: Biparser () () String
reservedop = munch' PB.munchReservedop PB.isReservedop

decimal :: Biparser () () String
decimal = some' digit

octal :: Biparser () () [Char]
octal = some' octit

hexadecimal :: Biparser () () String
hexadecimal = some' hexit

exponent :: Biparser () () String
exponent =
  (string' "e" <+> string' "E")
    <<#*>> opt (string' "+" <+> string' "-")
    <<#*>> decimal
    >>#> mkExponent
    >>#> validate PB.isExponent
  where
    mkExponent = partialIso f g
    f ((a, b), c) = Right $ a <> b <> c
    g (e : pm : d)
      | pm `elem` "+-" = Right (([e], [pm]), d)
      | otherwise = Right (([e], []), pm : d)
    g _ = error "exponent.print: bad input (impossible)"

varid :: Biparser () () String
varid = munch' PB.munchVarid PB.isVarid

varsym :: Biparser () () String
varsym = munch' PB.munchVarsym PB.isVarsym

conid :: Biparser () () String
conid = munch' PB.munchConid PB.isConid

consym :: Biparser () () String
consym = munch' PB.munchConsym PB.isConsym

tyvar :: Biparser () () String
tyvar = varid

tycon :: Biparser () () String
tycon = conid

tycls :: Biparser () () String
tycls = conid

modid :: Biparser () () String
modid = sepBy1'' conid (char '.') >#>> partialIso f g
  where
    f = pure . join . punctuate "."
    g s =
      if isModid s
        then Right $ splitList (/= '.') s
        else Left $ Error "modid.print: bad input"

-- >>> testParse modid () "F.Gh.H"
-- >>> testPrint modid ("F.Gh.H", Nothing)
-- Right (("F.Gh.H",()),("",[],Ln 1, Col 7))
-- Right ((),("F.Gh.H",[],Ln 1, Col 7))

isModid :: String -> Bool
isModid s =
  let (x, s') = span PB.munchConid s
   in PB.isConid x && case List.uncons s' of
        Nothing -> True
        Just (dot, xs) -> dot == '.' && isModid xs

qual :: Biparser () () String
qual = opt (modid <<*# char '.')

qvarid :: Biparser () () (String, String)
qvarid = qual <<#*>> varid

qconid :: Biparser () () (String, String)
qconid = qual <<#*>> conid

qtycon :: Biparser () () (String, String)
qtycon = qual <<#*>> tycon

qtycls :: Biparser () () (String, String)
qtycls = qual <<#*>> tycls

qvarsym :: Biparser () () (String, String)
qvarsym = qual <<#*>> varsym

-- >>> testParse (fully qvarsym) () "F."    -- fail
-- >>> testParse (fully qvarsym) () "F.."   -- ok
-- >>> testParse (fully qvarsym) () "F..."  -- fail
-- >>> testParse (fully qvarsym) () "F...." -- ok
-- Left validate: invalid value
-- Right ((("F","."),((),"")),("",[],Ln 1, Col 4))
-- Left validate: invalid value
-- Right ((("F","..."),((),"")),("",[],Ln 1, Col 6))

qconsym :: Biparser () () (String, String)
qconsym = qual <<#*>> consym

hCons :: Biparser () () String
hCons = munch' PB.munchCons PB.isCons

float :: Biparser Cfloat () String
float =
  mapC wrapCfloat unwrapCfloat $
    withDot </> withoutDot <?> "float"
  where
    withDot =
      decimal
        <<*# char '.'
        <<#*>> decimal
        <<#*>> opt exponent
        >>#> mkFloat1
    withoutDot =
      decimal
        <<#*>> exponent
        >>#> mkFloat2

-- >>> testParse float () "0.110e5"
-- >>> testPrint float ("0.110e5", Nothing)
-- >>> testPrint float ("0.110e5", Just CfloatWithDot)
-- >>> testPrint float ("0.110e5", Just CfloatWithoutDot) -- Is the wrong one...
-- Right (("0.110e5",CfloatWithDot),("",[],Ln 1, Col 8))
-- Right ((),("0.110e5",[],Ln 1, Col 8))
-- Right ((),("0.110e5",[],Ln 1, Col 8))
-- Right ((),("0.110e5",[],Ln 1, Col 8))

-- >>> testParse float () "12e6"
-- >>> testPrint float ("12e6", Nothing)
-- >>> testPrint float ("12e6", Just CfloatWithDot)
-- >>> testPrint float ("12e6", Just CfloatWithoutDot)
-- Right (("12e6",CfloatWithoutDot),("",[],Ln 1, Col 5))
-- Right ((),("12e6",[],Ln 1, Col 5))
-- Right ((),("12e6",[],Ln 1, Col 5))
-- Right ((),("12e6",[],Ln 1, Col 5))

mkFloat1 :: Biparser () ((String, String), String) String
mkFloat1 = partialIso f g
  where
    f ((d1, d2), e) = Right $ d1 <> "." <> d2 <> e
    g s1 =
      let (d1, s2) = span PB.isDigit s1
          (dot, s3) = span (== '.') s2
          (d2, e) = span PB.isDigit s3
       in if and
            [ PB.isDecimal d1,
              dot == ".",
              PB.isDecimal d2,
              PB.isExponent e || null e
            ]
            then Right ((d1, d2), e)
            else Left $ Error "mkFloat1.print: bad input"

-- >>> testParse mkFloat1 (("12", "34"), "e5") ""
-- >>> testPrint mkFloat1 ("12.34e5", Nothing)
-- Right (("12.34e5",()),("",[],Ln 1, Col 1))
-- Right ((("12","34"),"e5"),("",[],Ln 1, Col 1))

mkFloat2 :: Biparser () (String, String) String
mkFloat2 = partialIso f g
  where
    f (d, e) = Right $ d <> e
    g s =
      let (d, e) = span PB.isDigit s
       in if PB.isDecimal d && PB.isExponent e
            then Right (d, e)
            else Left $ Error "mkFloat2.print: bad input"

-- >>> testParse mkFloat2 ("12", "e-5") ""
-- >>> testPrint mkFloat2 ("12e-5", Nothing)
-- Right (("12e-5",()),("",[],Ln 1, Col 1))
-- Right (("12","e-5"),("",[],Ln 1, Col 1))

integer :: Biparser Cinteger () String
integer =
  mapC wrapCinteger unwrapCinteger $
    dec </> oct1 </> oct2 </> hex1 </> hex2 <?> "integer"
  where
    dec = decimal
    oct1 = string "0o" #*>> octal
    oct2 = string "0O" #*>> octal
    hex1 = string "0x" #*>> hexadecimal
    hex2 = string "0X" #*>> hexadecimal

hChar :: Biparser () () Char
hChar = char '\'' #*>> contents <<*# char '\'' <?> "hChar"
  where
    contents = satisfy (\c -> PB.isGraphic c && c /= '\'' && c /= '\\') <+> satisfy isSpace

hString :: Biparser () () String
hString = char '\"' #*>> contents <<*# char '\"' <?> "hString"
  where
    contents = many' (satisfy (\c -> PB.isGraphic c && c /= '\"' && c /= '\\') <+> satisfy isSpace)
