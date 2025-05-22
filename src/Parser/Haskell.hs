{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE FlexibleInstances #-}
{-# HLINT ignore "Use <$>" #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Parser.Haskell where

import Control.Applicative
import Control.Monad hiding (guard)
import qualified Data.Bifunctor as Bf
import qualified Data.List as List
import Language.Haskell.AST
import Parser
import Parser.Haskell.Basic
import Parser.Haskell.Token
import Utils
import Prelude hiding (any, exp, exponent)

fullModule :: P HsModule
fullModule = fully hModule

literal :: P HsLiteral
literal =
  token $
    choice
      [ HsFloat <$> float,
        HsInteger <$> integer,
        HsChar <$> hChar,
        HsString <$> hString
      ]

exp :: P HsExp
exp = choice [infixexp]

infixexp :: P HsExp
infixexp =
  choice
    [ infixopexp,
      lexp
    ]

infixopexp :: P HsExp
infixopexp = do
  e1 <- lexp
  op <- qop
  e2 <- infixexp
  pure (HsInfixApp e1 op e2)

lexp :: P HsExp
lexp =
  choice
    [ lambdaexp,
      letexp,
      ifexp,
      fexp,
      caseexp,
      HsNegApp <$> (minus *> notFollowedBy lambdaexp *> lexp)
    ]

ifexp :: P HsExp
ifexp = do
  _ <- kwIf
  e1 <- exp
  _ <- kwThen
  e2 <- exp
  _ <- kwElse
  e3 <- exp
  pure (HsIf e1 e2 e3)

letexp :: P HsExp
letexp = do
  _ <- kwLet
  ds <- decls
  _ <- kwIn
  e <- exp
  pure (HsLet ds e)

fexp :: P HsExp
fexp = f <$> some aexp
  where
    f :: [HsExp] -> HsExp
    f [] = error "impossible: got empty list"
    f (e : es) = foldl HsApp e es

lambdaexp :: P HsExp
lambdaexp = do
  _ <- backslash
  pats <- some apat
  _ <- rightArrow
  e <- exp
  pure (HsLambda pats e)

aexp :: P HsExp
aexp =
  choice
    [ HsVar <$> qvar,
      HsCon <$> gcon,
      HsLit <$> literal,
      HsParen <$> parens exp,
      HsList <$> brackets (some exp),
      leftsectionexp,
      rightsectionexp
    ]

leftsectionexp :: P HsExp
leftsectionexp = do
  _ <- openParen
  e <- infixexp
  op <- qop
  _ <- closeParen
  pure (HsLeftSection e op)

rightsectionexp :: P HsExp
rightsectionexp = do
  _ <- openParen
  op <- qop
  guardError (not (isNegate op)) "negate (-) cannot be used in right sections"
  e <- infixexp
  _ <- closeParen
  pure (HsRightSection op e)

unitCon :: P HsSpecialCon
unitCon = HsUnitCon <$ openParen <* closeParen

listCon :: P HsSpecialCon
listCon = HsListCon <$ openBracket <* closeBracket

tupleCon :: P HsSpecialCon
tupleCon = HsTupleCon . (+ 1) . length <$> (openParen *> some comma <* closeParen)

consCon :: P HsSpecialCon
consCon = HsCons <$ colon

gcon :: P HsQName
gcon =
  choice
    [ Special <$> unitCon,
      Special <$> listCon,
      Special <$> tupleCon,
      qcon
    ]

var :: P HsName
var =
  choice
    [ HsIdent <$> token varid,
      HsSymbol <$> parens (token varsym)
    ]

qvar :: P HsQName
qvar =
  uncurry Qual . Bf.first Module
    <$> choice
      [ Bf.second HsIdent <$> token qvarid,
        Bf.second HsSymbol <$> parens (token qvarsym)
      ]

con :: P HsQName
con =
  UnQual
    <$> choice
      [ HsIdent <$> token conid,
        HsSymbol <$> parens (token consym)
      ]

qcon :: P HsQName
qcon =
  choice
    [ uncurry Qual . Bf.bimap Module HsIdent <$> token qconid,
      uncurry Qual . Bf.bimap Module HsSymbol <$> parens (token qconsym),
      Special <$> parens consCon
    ]

varop :: P HsOp
varop =
  HsVarOp
    <$> choice
      [ HsSymbol <$> token varsym,
        HsIdent <$> backTicks (token varid)
      ]

qvarop :: P HsQOp
qvarop =
  HsQVarOp . uncurry Qual . Bf.first Module
    <$> choice
      [ Bf.second HsSymbol <$> token qvarsym,
        Bf.second HsIdent <$> backTicks (token qvarid)
      ]

conop :: P HsOp
conop =
  HsConOp
    <$> choice
      [ HsSymbol <$> token consym,
        HsIdent <$> backTicks (token conid)
      ]

qconop :: P HsQOp
qconop =
  HsQConOp
    <$> choice
      [ uncurry Qual . Bf.bimap Module HsSymbol <$> token qconsym,
        uncurry Qual . Bf.bimap Module HsIdent <$> backTicks (token qconid),
        Special <$> consCon
      ]

op :: P HsOp
op = varop <|> conop

qop :: P HsQOp
qop = qvarop <|> qconop

pat :: P HsPat
pat =
  choice
    [ infixpat,
      lpat
    ]

infixpat :: P HsPat
infixpat = do
  p1 <- lpat
  op <- qconop
  p2 <- pat
  pure (HsPInfixApp p1 op p2)

lpat :: P HsPat
lpat =
  choice
    [ apat,
      HsPApp <$> gcon <*> some apat
    ]

apat :: P HsPat
apat =
  choice
    [ HsPVar <$> var,
      HsPApp <$> gcon <*> pure [],
      HsPLit <$> literal,
      HsPWildCard <$ underscore,
      HsPParen <$> parens pat
    ]

hModule :: P HsModule
hModule =
  choice
    [ simpleModule
    ]

simpleModule :: P HsModule
simpleModule = do
  ds <- body
  pure (HsModule (Module "") Nothing [] ds)

body :: P [HsDecl]
body =
  choice
    [ topdecls
    ]

topdecls :: P [HsDecl]
topdecls = block1 topdecl

topdecl :: P HsDecl
topdecl = choice [decl]

decls :: P [HsDecl]
decls = block0 decl

decl :: P HsDecl
decl =
  choice
    [ gendecl,
      patdecl,
      fundecl
    ]

gendecl :: P HsDecl
gendecl =
  choice
    [ typesigdecl,
      fixitydecl
    ]

ops :: P [HsOp]
ops = sepBy1 op comma

vars :: P [HsName]
vars = sepBy1 var comma

fixity :: P HsAssoc
fixity =
  choice
    [ HsAssocLeft <$ kwInfixl,
      HsAssocRight <$ kwInfixr,
      HsAssocNone <$ kwInfix
    ]

fixitydecl :: P HsDecl
fixitydecl = do
  f <- fixity
  d <- option '9' digit
  case digitToInt d of
    Nothing -> fail $ "expected digit, got: " ++ show d
    Just i -> do
      o <- ops
      pure (HsInfixDecl f i o)

typesigdecl :: P HsDecl
typesigdecl = do
  vs <- vars
  _ <- doubleColon
  ctxt <- option [] (context <* bigRightArrow)
  t <- hType
  let qt = HsQualType ctxt t
  pure (HsTypeSig vs qt)

hType :: P HsType
hType = f <$> sepBy1 btype rightArrow
  where
    f :: [HsType] -> HsType
    f [] = error "impossible: got empty list"
    f [t] = t
    f (t : ts) = HsTyFun t (f ts)

btype :: P HsType
btype = f <$> some atype
  where
    f :: [HsType] -> HsType
    f [] = error "impossible: got empty list"
    f (t : ts) = foldl HsTyApp t ts

atype :: P HsType
atype =
  choice
    [ gtycon,
      HsTyVar . HsIdent <$> token tyvar,
      HsTyTuple <$> parens (sepBy2 hType comma),
      HsTyAppList <$> brackets hType,
      HsTyParen <$> parens hType
    ]

gtycon :: P HsType
gtycon =
  (HsTyCon <$>) $
    choice
      [ uncurry Qual . Bf.bimap Module HsIdent <$> token qtycon,
        Special <$> unitCon,
        Special <$> listCon,
        Special <$> funCon,
        Special <$> tupleCon
      ]

funCon :: P HsSpecialCon
funCon = HsFunCon <$ parens rightArrow

context :: P HsContext
context =
  choice
    [ List.singleton <$> hClass,
      parens (sepBy0 hClass comma)
    ]

hClass :: P HsAsst
hClass = choice [hClassSingle, hClassMultiple]

hClassSingle :: P HsAsst
hClassSingle = do
  cls <- pqtycls
  tyv <- ptyvar
  pure (cls, List.singleton tyv)

hClassMultiple :: P HsAsst
hClassMultiple = do
  cls <- pqtycls
  _ <- openParen
  tyv <- ptyvar
  atyps <- some atype
  _ <- closeParen
  pure (cls, tyv : atyps)

pqtycls :: P HsQName
pqtycls = uncurry Qual . Bf.bimap Module HsIdent <$> token qtycls

ptyvar :: P HsType
ptyvar = HsTyVar . HsIdent <$> token tyvar

patdecl :: P HsDecl
patdecl = do
  p <- pat
  (r, wdecls) <- rhs
  pure (HsPatBind p r wdecls)

fundecl :: P HsDecl
fundecl = do
  (v, ps) <- funlhs
  (r, wdecls) <- rhs
  pure (HsFunBind v ps r wdecls)

funlhs :: P (HsName, [HsPat])
funlhs =
  choice
    [ (,) <$> var <*> some apat
    ]

rhs :: P (HsRhs, HsWhere)
rhs =
  (,)
    <$> choice
      [ HsUnguardedRhs <$> (equals *> exp)
      ]
    <*> whereDecls

whereDecls :: P HsWhere
whereDecls = (kwWhere *> decls) <|> pure []

caseexp :: P HsExp
caseexp = do
  _ <- kwCase
  e <- exp
  _ <- kwOf
  as <- alts
  pure (HsCase e as)

alts :: P [HsAlt]
alts = block1 alt

alt :: P HsAlt
alt =
  choice
    [ alt1,
      alt2
    ]

alt1 :: P HsAlt
alt1 = do
  p <- pat
  _ <- rightArrow
  e <- exp
  ds <- whereDecls
  pure (HsAlt p (HsUnguardedAlt e) ds)

alt2 :: P HsAlt
alt2 = do
  p <- pat
  gp <- gdpat
  ds <- whereDecls
  pure (HsAlt p (HsGuardedAlts gp) ds)

gdpat :: P [HsGuardedAlt]
gdpat = some $ do
  _ <- pipe
  e1 <- exp -- simplified from 'guards'
  _ <- rightArrow
  e2 <- exp
  pure (HsGuardedAlt e1 e2)
