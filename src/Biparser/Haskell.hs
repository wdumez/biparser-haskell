{-# LANGUAGE EmptyDataDeriving #-}
{-# LANGUAGE RankNTypes #-}

module Biparser.Haskell where

import Biparser
import Biparser.Haskell.Basic
import Biparser.Haskell.Complements
import Biparser.Haskell.Constructors
import Biparser.Haskell.Token
import Data.Maybe (isNothing)
import Language.Haskell.AST
import PErr
import Utils
import Prelude hiding (exp)

fullModule :: Biparser (ChModule, String) () HsModule
fullModule = fully hModule

literal :: Biparser Cliteral () HsLiteral
literal =
  token $
    mapC wrapCliteral' unwrapCliteral' $
      (float >>#> mkHsFloat)
        </> (integer >>#> mkHsInteger)
        </> (hChar >>#> mkHsChar)
        </> (hString >>#> mkHsString)
        <?> "literal"

exp :: Biparser Cexp () HsExp
exp = infixexp <?> "exp"

infixexp :: Biparser Cinfixexp () HsExp
infixexp =
  mapC wrapCinfixexp unwrapCinfixexp $
    infixopexp
      </> lexp
      <?> "infixexp"

infixopexp :: Biparser Cinfixopexp () HsExp
infixopexp =
  lexp
    <<*>> qop
    <<*>> infixexp
    >>#> mkHsInfixApp
    <?> "infixopexp"

lexp :: Biparser Clexp () HsExp
lexp =
  mapC wrapClexp unwrapClexp $
    lambdaexp
      </> letexp
      </> ifexp
      </> fexp
      </> caseexp
      </> (minus <<*# notFollowedBy (parse lambdaexp) *>> lexp >>#> mkHsNegApp)
      <?> "lexp"

ifexp :: Biparser Cifexp () HsExp
ifexp =
  kwIf
    *>> exp
    <<* kwThen
    <<*>> exp
    <<* kwElse
    <<*>> exp
    >>#> mkHsIf
    <?> "ifexp"

letexp :: Biparser Cletexp () HsExp
letexp =
  kwLet
    *>> decls
    <<* kwIn
    <<*>> exp
    >>#> mkHsLet
    <?> "letexp"

fexp :: Biparser Cfexp () HsExp
fexp = (some aexp >>#> mkHsApp') <?> "fexp"

lambdaexp :: Biparser Clambdaexp () HsExp
lambdaexp =
  backslash
    *>> some apat
    <<* rightArrow
    <<*>> exp
    >>#> mkHsLambda
    <?> "lambdaexp"

caseexp :: Biparser Ccaseexp () HsExp
caseexp =
  kwCase
    *>> exp
    <<* kwOf
    <<*>> alts
    >>#> mkHsCase
    <?> "caseexp"

aexp :: Biparser Caexp () HsExp
aexp =
  mapC wrapCaexp unwrapCaexp $
    (qvar >>#> mkHsVar)
      </> (gcon >>#> mkHsCon)
      </> (literal >>#> mkHsLit)
      </> (parens exp >>#> mkHsParen)
      </> (brackets (some exp) >>#> mkHsList)
      </> leftsectionexp
      </> rightsectionexp
      <?> "aexp"

leftsectionexp :: Biparser Cleftsectionexp () HsExp
leftsectionexp =
  openParen
    *>> infixexp
    <<*>> qop
    <<* closeParen
    >>#> mkHsLeftSection
    <?> "leftsectionexp"

rightsectionexp :: Biparser Crightsectionexp () HsExp
rightsectionexp =
  openParen
    *>> (qop >>#> (validate (not . isNegate) <?> "negate (-) symbol"))
    <<*>> infixexp
    <<* closeParen
    >>#> mkHsRightSection
    <?> "rightsectionexp"

unitCon :: Biparser CunitCon () HsSpecialCon
unitCon = openParen *>> closeParen >>#> mkHsUnitCon <?> "unitCon"

listCon :: Biparser ClistCon () HsSpecialCon
listCon = openBracket *>> closeBracket >>#> mkHsListCon <?> "listCon"

consCon :: Biparser CconsCon () HsSpecialCon
consCon = colon *#>> mkHsCons <?> "consCon"

tupleCon :: Biparser CtupleCon () HsSpecialCon
tupleCon =
  openParen
    *>> some comma
    <<* closeParen
    >>#> iso ((+ 1) . length) ((`replicate` ()) . (\x -> x - 1))
    >>#> mkHsTupleCon
    <?> "tupleCon"

gcon :: Biparser Cgcon () HsQName
gcon =
  mapC wrapCgcon unwrapCgcon $
    (unitCon >>#> mkSpecial)
      </> (listCon >>#> mkSpecial)
      </> (tupleCon >>#> mkSpecial)
      </> qcon
      <?> "gcon"

var :: Biparser Cvar () HsName
var =
  mapC wrapCvar unwrapCvar $
    (token' varid >>#> mkHsIdent)
      </> (parens (token' varsym) >>#> mkHsSymbol)
      <?> "var"

qvar :: Biparser Cqvar () HsQName
qvar =
  mapC wrapCqvar unwrapCqvar $
    ( (token' qvarid >>#> second mkHsIdent)
        </> (parens (token' qvarsym) >>#> second mkHsSymbol)
    )
      >>#> first mkModule
      >>#> mkQual
      <?> "qvar"

con :: Biparser Ccon () HsQName
con =
  mapC wrapCcon unwrapCcon $
    ( (token' conid >>#> mkHsIdent)
        </> (parens (token' consym) >>#> mkHsSymbol)
    )
      >>#> mkUnQual
      <?> "con"

qcon :: Biparser Cqcon () HsQName
qcon =
  mapC wrapCqcon unwrapCqcon $
    (token' qconid >>#> mkModule **#* mkHsIdent >>#> mkQual)
      </> (parens (token' qconsym) >>#> mkModule **#* mkHsSymbol >>#> mkQual)
      </> (parens consCon >>#> mkSpecial)
      <?> "qcon"

varop :: Biparser Cvarop () HsOp
varop =
  mapC wrapCvarop unwrapCvarop $
    ( (token' varsym >>#> mkHsSymbol)
        </> (backTicks (token' varid) >>#> mkHsIdent)
    )
      >>#> mkHsVarOp
      <?> "qvarop"

qvarop :: Biparser Cqvarop () HsQOp
qvarop =
  mapC wrapCqvarop unwrapCqvarop $
    ( (token' qvarsym >>#> second mkHsSymbol)
        </> (backTicks (token' qvarid) >>#> second mkHsIdent)
    )
      >>#> first mkModule
      >>#> mkQual
      >>#> mkHsQVarOp
      <?> "qvarop"

conop :: Biparser Cconop () HsOp
conop =
  mapC wrapCconop unwrapCconop $
    ( (token' consym >>#> mkHsSymbol)
        </> (backTicks (token' conid) >>#> mkHsIdent)
    )
      >>#> mkHsConOp
      <?> "conop"

qconop :: Biparser Cqconop () HsQOp
qconop =
  mapC wrapCqconop unwrapCqconop $
    ( (token' qconsym >>#> mkModule **#* mkHsSymbol >>#> mkQual)
        </> (backTicks (token' qconid) >>#> mkModule **#* mkHsIdent >>#> mkQual)
        </> (consCon >>#> mkSpecial)
    )
      >>#> mkHsQConOp
      <?> "qconop"

op :: Biparser Cop () HsOp
op =
  mapC wrapCop unwrapCop $
    varop </> conop <?> "op"

qop :: Biparser Cqop () HsQOp
qop =
  mapC wrapCqop unwrapCqop $
    qvarop </> qconop <?> "qop"

pat :: Biparser Cpat () HsPat
pat =
  mapC wrapCpat unwrapCpat $
    infixpat </> lpat <?> "pat"

infixpat :: Biparser Cinfixpat () HsPat
infixpat =
  lpat
    <<*>> qconop
    <<*>> pat
    >>#> mkHsPInfixApp
    <?> "infixpat"

lpat :: Biparser Clpat () HsPat
lpat =
  mapC wrapClpat unwrapClpat $
    (gcon <<*>> some apat >>#> mkHsPApp)
      </> apat
      <?> "lpat"

apat :: Biparser Capat () HsPat
apat =
  mapC wrapCapat unwrapCapat $
    (var >>#> mkHsPVar)
      </> (gcon <<*#>> constant [] null >>#> mkHsPApp)
      </> (literal >>#> mkHsPLit)
      </> (underscore >>#> mkHsPWildCard)
      </> (parens pat >>#> mkHsPParen)
      <?> "apat"

hModule :: Biparser ChModule () HsModule
hModule = simpleModule <?> "hModule"

simpleModule :: Biparser CsimpleModule () HsModule
simpleModule =
  (constant' "" >>#> mkModule)
    <<#*>> constant Nothing isNothing
    <<#*>> constant [] null
    <<#*>> body
    >>#> mkHsModule
    <?> "simpleModule"

body :: Biparser Cbody () [HsDecl]
body = topdecls <?> "body"

topdecls :: Biparser Ctopdecls () [HsDecl]
topdecls = block1 topdecl <?> "topdecls"

topdecl :: Biparser Ctopdecl () HsDecl
topdecl = decl <?> "topdecl"

decls :: Biparser Cdecls () [HsDecl]
decls = block0 decl <?> "decls"

decl :: Biparser Cdecl () HsDecl
decl =
  mapC wrapCdecl unwrapCdecl $
    gendecl
      </> patdecl
      </> fundecl
      <?> "decl"

gendecl :: Biparser Cgendecl () HsDecl
gendecl =
  mapC wrapCgendecl unwrapCgendecl $
    typesigdecl
      </> fixitydecl
      <?> "gendecl"

patdecl :: Biparser Cpatdecl () HsDecl
patdecl =
  pat
    <<*>> rhs
    >>#> iso (\(a, (b, c)) -> ((a, b), c)) (\((a, b), c) -> (a, (b, c)))
    >>#> mkHsPatBind
    <?> "patdecl"

fundecl :: Biparser Cfundecl () HsDecl
fundecl =
  funlhs
    <<*>> rhs
    >>#> iso (\(a, (b, c)) -> ((a, b), c)) (\((a, b), c) -> (a, (b, c)))
    >>#> mkHsFunBind
    <?> "fundecl"

ops :: Biparser Cops () [HsOp]
ops = sepBy1 op comma <?> "ops"

vars :: Biparser Cvars () [HsName]
vars = sepBy1 var comma <?> "vars"

fixity :: Biparser Cfixity () HsAssoc
fixity =
  mapC wrapCfixity unwrapCfixity $
    (kwInfixl >>#> mkHsAssocLeft)
      </> (kwInfixr >>#> mkHsAssocRight)
      </> (kwInfix >>#> mkHsAssocNone)
      <?> "fixity"

fixitydecl :: Biparser Cfixitydecl () HsDecl
fixitydecl =
  fixity
    <<*#>> (option '9' digit >>#> partialIso f g)
    <<*>> ops
    >>#> mkHsInfixDecl
    <?> "fixitydecl"
  where
    f c = case digitToInt c of
      Nothing -> Left $ Error "not a digit"
      Just i -> Right i
    g i = case intToDigit i of
      Nothing -> Left $ Error "not a digit"
      Just c -> Right c

typesigdecl :: Biparser Ctypesigdecl () HsDecl
typesigdecl =
  vars
    <<* doubleColon
    <<*>> ( fullcontext
              <<*>> hType
              >>#> mkHsQualType
          )
    >>#> mkHsTypeSig
    <?> "typesigdecl"

hType :: Biparser ChType () HsType
hType =
  sepBy1 btype rightArrow
    >>#> mkHsTyFun'
    <?> "hType"

btype :: Biparser Cbtype () HsType
btype = some atype >>#> mkHsTyApp' <?> "btype"

atype :: Biparser Catype () HsType
atype =
  mapC wrapCatype unwrapCatype $
    gtycon
      </> (token' tyvar >>#> mkHsIdent >>#> mkHsTyVar)
      </> (parens (sepBy2 hType comma) >>#> mkHsTyTuple)
      </> (brackets hType >>#> mkHsTyAppList)
      </> (parens hType >>#> mkHsTyParen)
      <?> "atype"

gtycon :: Biparser Cgtycon () HsType
gtycon =
  mapC wrapCgtycon unwrapCgtycon $
    ( (token' qtycon >>#> mkModule **#* mkHsIdent >>#> mkQual)
        </> (unitCon >>#> mkSpecial)
        </> (listCon >>#> mkSpecial)
        </> (funCon >>#> mkSpecial)
        </> (tupleCon >>#> mkSpecial)
    )
      >>#> mkHsTyCon
      <?> "gtycon"

funCon :: Biparser CfunCon () HsSpecialCon
funCon = parens rightArrow >>#> mkHsFunCon <?> "funCon"

context :: Biparser Ccontext () [(HsQName, [HsType])]
context =
  mapC wrapCcontext unwrapCcontext $
    (hClass >>#> singleton)
      </> parens (sepBy0 hClass comma)
      <?> "context"

fullcontext :: Biparser Cfullcontext () [(HsQName, [HsType])]
fullcontext =
  mapC wrapCfullcontext unwrapCfullcontext $
    (context <<* bigRightArrow) </> constant [] null

hClass :: Biparser ChClass () (HsQName, [HsType])
hClass = hClassSingle </> hClassMultiple <?> "hClass"

hClassSingle :: Biparser ChClassSingle () (HsQName, [HsType])
hClassSingle =
  pqtycls
    <<*>> (ptyvar >>#> singleton)
    <?> "hClassSingle"

hClassMultiple :: Biparser ChClassMultiple () (HsQName, [HsType])
hClassMultiple =
  pqtycls
    <<* openParen
    <<*>> (ptyvar <<*>> some atype >>#> mkCons)
    <<* closeParen
    <?> "hClassMultiple"

pqtycls :: Biparser Cptycls () HsQName
pqtycls = token' qtycls >>#> mkModule **#* mkHsIdent >>#> mkQual <?> "pqtycls"

ptyvar :: Biparser Cptyvar () HsType
ptyvar = token' tyvar >>#> mkHsIdent >>#> mkHsTyVar <?> "ptyvar"

funlhs :: Biparser Cfunlhs () (HsName, [HsPat])
funlhs = var <<*>> some apat <?> "funlhs"

rhs :: Biparser Crhs () (HsRhs, [HsDecl])
rhs =
  (equals *>> exp >>#> mkHsUnguardedRhs)
    <<*>> whereDecls
    <?> "rhs"

whereDecls :: Biparser CwhereDecls () [HsDecl]
whereDecls = (notFollowedBy_ (parse kwWhere) #*>> constant [] null) </> (kwWhere *>> decls)

alts :: Biparser Calts () [HsAlt]
alts = block1 alt <?> "alts"

alt :: Biparser Calt () HsAlt
alt = alt1 </> alt2 <?> "alt"

alt1 :: Biparser Calt1 () HsAlt
alt1 =
  pat
    <<* rightArrow
    <<*>> (exp >>#> mkHsUnguardedAlt)
    <<*>> whereDecls
    >>#> mkHsAlt
    <?> "alt1"

alt2 :: Biparser Calt2 () HsAlt
alt2 =
  pat
    <<*>> (gdpat >>#> mkHsGuardedAlts)
    <<*>> whereDecls
    >>#> mkHsAlt
    <?> "alt2"

gdpat :: Biparser Cgdpat () [HsGuardedAlt]
gdpat =
  some $
    pipe
      *>> exp
      <<* rightArrow
      <<*>> exp
      >>#> mkHsGuardedAlt
      <?> "gdpat"
