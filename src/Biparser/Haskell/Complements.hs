module Biparser.Haskell.Complements where

import Biparser.Indentation (CBlock, CLineFold)

{-
Complement types.

Many of these types are reified (with newtype of data) to avoid circular complements.
These data types and especially their wrap- and unwrap functions can probably be generated automatically.
-}

type CToken = CLineFold

type CParens c = (c, (CToken, CToken))

type CBrackets c = (c, (CToken, CToken))

type CBackTicks c = (c, (CToken, CToken))

data Cfloat
  = CfloatWithDot
  | CfloatWithoutDot
  deriving (Show, Eq, Ord)

wrapCfloat :: Either () () -> Cfloat
wrapCfloat (Left ()) = CfloatWithDot
wrapCfloat (Right ()) = CfloatWithoutDot

unwrapCfloat :: Cfloat -> Either () ()
unwrapCfloat CfloatWithDot = Left ()
unwrapCfloat CfloatWithoutDot = Right ()

data Cinteger
  = CintegerDecimal
  | CintegerOct1
  | CintegerOct2
  | CintegerHex1
  | CintegerHex2
  deriving (Show, Eq, Ord)

wrapCinteger :: Either (Either (Either (Either () ()) ()) ()) () -> Cinteger
wrapCinteger (Right ()) = CintegerHex2
wrapCinteger (Left (Right ())) = CintegerHex1
wrapCinteger (Left (Left (Right ()))) = CintegerOct2
wrapCinteger (Left (Left (Left (Right ())))) = CintegerOct1
wrapCinteger (Left (Left (Left (Left ())))) = CintegerDecimal

unwrapCinteger :: Cinteger -> Either (Either (Either (Either () ()) ()) ()) ()
unwrapCinteger CintegerHex2 = Right ()
unwrapCinteger CintegerHex1 = Left (Right ())
unwrapCinteger CintegerOct2 = Left (Left (Right ()))
unwrapCinteger CintegerOct1 = Left (Left (Left (Right ())))
unwrapCinteger CintegerDecimal = Left (Left (Left (Left ())))

data Cliteral'
  = CliteralFloat Cfloat
  | CliteralInteger Cinteger
  | CliteralChar
  | CliteralString
  deriving (Show, Eq, Ord)

wrapCliteral' :: Either (Either (Either Cfloat Cinteger) ()) () -> Cliteral'
wrapCliteral' (Right ()) = CliteralString
wrapCliteral' (Left (Right ())) = CliteralChar
wrapCliteral' (Left (Left (Right c))) = CliteralInteger c
wrapCliteral' (Left (Left (Left c))) = CliteralFloat c

unwrapCliteral' :: Cliteral' -> Either (Either (Either Cfloat Cinteger) ()) ()
unwrapCliteral' CliteralString = Right ()
unwrapCliteral' CliteralChar = Left (Right ())
unwrapCliteral' (CliteralInteger c) = Left (Left (Right c))
unwrapCliteral' (CliteralFloat c) = Left (Left (Left c))

type Cliteral = (Cliteral', CToken)

type Cexp = Cinfixexp

-- Note: this newtype wrapper is only required to eliminate infinite recursion for the types.
newtype Cinfixexp = Cinfixexp (Either Cinfixopexp Clexp)
  deriving (Show, Eq, Ord)

wrapCinfixexp :: Either Cinfixopexp Clexp -> Cinfixexp
wrapCinfixexp = Cinfixexp

unwrapCinfixexp :: Cinfixexp -> Either Cinfixopexp Clexp
unwrapCinfixexp (Cinfixexp e) = e

type Cinfixopexp = ((Clexp, Cqop), Cinfixexp)

data Clexp
  = ClexpLambdaexp Clambdaexp
  | ClexpLetexp Cletexp
  | ClexpIfexp Cifexp
  | ClexpFexp Cfexp
  | ClexpCaseexp Ccaseexp
  | ClexpNeg (CToken, Clexp)
  deriving (Show, Eq, Ord)

wrapClexp :: Either (Either (Either (Either (Either Clambdaexp Cletexp) Cifexp) Cfexp) Ccaseexp) (CToken, Clexp) -> Clexp
wrapClexp (Right c) = ClexpNeg c
wrapClexp (Left (Right c)) = ClexpCaseexp c
wrapClexp (Left (Left (Right c))) = ClexpFexp c
wrapClexp (Left (Left (Left (Right c)))) = ClexpIfexp c
wrapClexp (Left (Left (Left (Left (Right c))))) = ClexpLetexp c
wrapClexp (Left (Left (Left (Left (Left c))))) = ClexpLambdaexp c

unwrapClexp :: Clexp -> Either (Either (Either (Either (Either Clambdaexp Cletexp) Cifexp) Cfexp) Ccaseexp) (CToken, Clexp)
unwrapClexp (ClexpNeg c) = Right c
unwrapClexp (ClexpCaseexp c) = Left (Right c)
unwrapClexp (ClexpFexp c) = Left (Left (Right c))
unwrapClexp (ClexpIfexp c) = Left (Left (Left (Right c)))
unwrapClexp (ClexpLetexp c) = Left (Left (Left (Left (Right c))))
unwrapClexp (ClexpLambdaexp c) = Left (Left (Left (Left (Left c))))

type Cifexp = (((((CToken, Cexp), CToken), Cexp), CToken), Cexp)

type Cletexp = (((CToken, Cdecls), CToken), Cexp)

type Cfexp = [Maybe Caexp]

type Clambdaexp = (((CToken, [Maybe Capat]), CToken), Cexp)

type Ccaseexp = (((CToken, Cexp), CToken), Calts)

data Caexp
  = CaexpQvar Cqvar
  | CaexpGcon Cgcon
  | CaexpLiteral Cliteral
  | CaexpParensExp (CParens Cexp)
  | CaexpList (CBrackets [Maybe Cexp])
  | CaexpLeftsection Cleftsectionexp
  | CaexpRightsection Crightsectionexp
  deriving (Show, Eq, Ord)

wrapCaexp :: Either (Either (Either (Either (Either (Either Cqvar Cgcon) Cliteral) (CParens Cexp)) (CBrackets [Maybe Cexp])) Cleftsectionexp) Crightsectionexp -> Caexp
wrapCaexp (Right c) = CaexpRightsection c
wrapCaexp (Left (Right c)) = CaexpLeftsection c
wrapCaexp (Left (Left (Right c))) = CaexpList c
wrapCaexp (Left (Left (Left (Right c)))) = CaexpParensExp c
wrapCaexp (Left (Left (Left (Left (Right c))))) = CaexpLiteral c
wrapCaexp (Left (Left (Left (Left (Left (Right c)))))) = CaexpGcon c
wrapCaexp (Left (Left (Left (Left (Left (Left c)))))) = CaexpQvar c

unwrapCaexp :: Caexp -> Either (Either (Either (Either (Either (Either Cqvar Cgcon) Cliteral) (CParens Cexp)) (CBrackets [Maybe Cexp])) Cleftsectionexp) Crightsectionexp
unwrapCaexp (CaexpRightsection c) = Right c
unwrapCaexp (CaexpLeftsection c) = Left (Right c)
unwrapCaexp (CaexpList c) = Left (Left (Right c))
unwrapCaexp (CaexpParensExp c) = Left (Left (Left (Right c)))
unwrapCaexp (CaexpLiteral c) = Left (Left (Left (Left (Right c))))
unwrapCaexp (CaexpGcon c) = Left (Left (Left (Left (Left (Right c)))))
unwrapCaexp (CaexpQvar c) = Left (Left (Left (Left (Left (Left c)))))

type Cleftsectionexp = (((CToken, Cinfixexp), Cqop), CToken)

type Crightsectionexp = (((CToken, Cqop), Cinfixexp), CToken)

type CunitCon = (CToken, CToken)

type ClistCon = (CToken, CToken)

type CconsCon = CToken

type CtupleCon = ((CToken, [Maybe CToken]), CToken)

data Cgcon
  = CgconUnitCon CunitCon
  | CgconListCon ClistCon
  | CgconTupleCon CtupleCon
  | CgconQcon Cqcon
  deriving (Show, Eq, Ord)

wrapCgcon :: Either (Either (Either CunitCon ClistCon) CtupleCon) Cqcon -> Cgcon
wrapCgcon = either (either (either CgconUnitCon CgconListCon) CgconTupleCon) CgconQcon

unwrapCgcon :: Cgcon -> Either (Either (Either CunitCon ClistCon) CtupleCon) Cqcon
unwrapCgcon (CgconQcon c) = Right c
unwrapCgcon (CgconTupleCon c) = Left (Right c)
unwrapCgcon (CgconListCon c) = Left (Left (Right c))
unwrapCgcon (CgconUnitCon c) = Left (Left (Left c))

data Cvar
  = CvarVarid CToken
  | CvarVarsym (CParens CToken)
  deriving (Show, Eq, Ord)

wrapCvar :: Either CToken (CParens CToken) -> Cvar
wrapCvar = either CvarVarid CvarVarsym

unwrapCvar :: Cvar -> Either CToken (CParens CToken)
unwrapCvar (CvarVarid c) = Left c
unwrapCvar (CvarVarsym c) = Right c

data Cqvar
  = CqvarQvarid CToken
  | CqvarQvarsym (CParens CToken)
  deriving (Show, Eq, Ord)

wrapCqvar :: Either CToken (CParens CToken) -> Cqvar
wrapCqvar = either CqvarQvarid CqvarQvarsym

unwrapCqvar :: Cqvar -> Either CToken (CParens CToken)
unwrapCqvar (CqvarQvarid c) = Left c
unwrapCqvar (CqvarQvarsym c) = Right c

data Ccon
  = CconConid CToken
  | CconConsym (CParens CToken)
  deriving (Show, Eq, Ord)

wrapCcon :: Either CToken (CParens CToken) -> Ccon
wrapCcon = either CconConid CconConsym

unwrapCcon :: Ccon -> Either CToken (CParens CToken)
unwrapCcon (CconConid c) = Left c
unwrapCcon (CconConsym c) = Right c

data Cqcon
  = CqconQconid CToken
  | CqconQconsym (CParens CToken)
  | CqconConsCon (CParens CconsCon)
  deriving (Show, Eq, Ord)

wrapCqcon :: Either (Either CToken (CParens CToken)) (CParens CconsCon) -> Cqcon
wrapCqcon = either (either CqconQconid CqconQconsym) CqconConsCon

unwrapCqcon :: Cqcon -> Either (Either CToken (CParens CToken)) (CParens CconsCon)
unwrapCqcon (CqconConsCon c) = Right c
unwrapCqcon (CqconQconsym c) = Left (Right c)
unwrapCqcon (CqconQconid c) = Left (Left c)

data Cvarop
  = CvaropVarsym CToken
  | CvaropVarid (CBackTicks CToken)
  deriving (Show, Eq, Ord)

wrapCvarop :: Either CToken (CBackTicks CToken) -> Cvarop
wrapCvarop = either CvaropVarsym CvaropVarid

unwrapCvarop :: Cvarop -> Either CToken (CBackTicks CToken)
unwrapCvarop (CvaropVarsym c) = Left c
unwrapCvarop (CvaropVarid c) = Right c

data Cqvarop
  = CqvaropQvarsym CToken
  | CqvaropQvarid (CBackTicks CToken)
  deriving (Show, Eq, Ord)

wrapCqvarop :: Either CToken (CBackTicks CToken) -> Cqvarop
wrapCqvarop = either CqvaropQvarsym CqvaropQvarid

unwrapCqvarop :: Cqvarop -> Either CToken (CBackTicks CToken)
unwrapCqvarop (CqvaropQvarsym c) = Left c
unwrapCqvarop (CqvaropQvarid c) = Right c

data Cconop
  = CconopConsym CToken
  | CconopConid (CBackTicks CToken)
  deriving (Show, Eq, Ord)

wrapCconop :: Either CToken (CBackTicks CToken) -> Cconop
wrapCconop = either CconopConsym CconopConid

unwrapCconop :: Cconop -> Either CToken (CBackTicks CToken)
unwrapCconop (CconopConsym c) = Left c
unwrapCconop (CconopConid c) = Right c

data Cqconop
  = CqconopQconsym CToken
  | CqconopQconid (CBackTicks CToken)
  | CqconopConsCon CconsCon
  deriving (Show, Eq, Ord)

wrapCqconop :: Either (Either CToken (CBackTicks CToken)) CconsCon -> Cqconop
wrapCqconop = either (either CqconopQconsym CqconopQconid) CqconopConsCon

unwrapCqconop :: Cqconop -> Either (Either CToken (CBackTicks CToken)) CconsCon
unwrapCqconop (CqconopQconsym c) = Left (Left c)
unwrapCqconop (CqconopQconid c) = Left (Right c)
unwrapCqconop (CqconopConsCon c) = Right c

data Cop
  = CopVarop Cvarop
  | CopConop Cconop
  deriving (Show, Eq, Ord)

wrapCop :: Either Cvarop Cconop -> Cop
wrapCop = either CopVarop CopConop

unwrapCop :: Cop -> Either Cvarop Cconop
unwrapCop (CopVarop c) = Left c
unwrapCop (CopConop c) = Right c

data Cqop
  = CqopQvarop Cqvarop
  | CqopQconop Cqconop
  deriving (Show, Eq, Ord)

wrapCqop :: Either Cqvarop Cqconop -> Cqop
wrapCqop = either CqopQvarop CqopQconop

unwrapCqop :: Cqop -> Either Cqvarop Cqconop
unwrapCqop (CqopQvarop c) = Left c
unwrapCqop (CqopQconop c) = Right c

data Cpat
  = CpatInfixpat Cinfixpat
  | CpatLpat Clpat
  deriving (Show, Eq, Ord)

wrapCpat :: Either Cinfixpat Clpat -> Cpat
wrapCpat = either CpatInfixpat CpatLpat

unwrapCpat :: Cpat -> Either Cinfixpat Clpat
unwrapCpat (CpatInfixpat c) = Left c
unwrapCpat (CpatLpat c) = Right c

type Cinfixpat = ((Clpat, Cqconop), Cpat)

data Clpat
  = ClpatOne Capat
  | ClpatMany (Cgcon, [Maybe Capat])
  deriving (Show, Eq, Ord)

wrapClpat :: Either (Cgcon, [Maybe Capat]) Capat -> Clpat
wrapClpat = either ClpatMany ClpatOne

unwrapClpat :: Clpat -> Either (Cgcon, [Maybe Capat]) Capat
unwrapClpat (ClpatOne c) = Right c
unwrapClpat (ClpatMany c) = Left c

data Capat
  = CapatVar Cvar
  | CapatGcon Cgcon
  | CapatLiteral Cliteral
  | CapatWildCard CToken
  | CapatParensPat (CParens Cpat)
  deriving (Show, Eq, Ord)

wrapCapat :: Either (Either (Either (Either Cvar Cgcon) Cliteral) CToken) (CParens Cpat) -> Capat
wrapCapat = either (either (either (either CapatVar CapatGcon) CapatLiteral) CapatWildCard) CapatParensPat

unwrapCapat :: Capat -> Either (Either (Either (Either Cvar Cgcon) Cliteral) CToken) (CParens Cpat)
unwrapCapat (CapatVar c) = Left (Left (Left (Left c)))
unwrapCapat (CapatGcon c) = Left (Left (Left (Right c)))
unwrapCapat (CapatLiteral c) = Left (Left (Right c))
unwrapCapat (CapatWildCard c) = Left (Right c)
unwrapCapat (CapatParensPat c) = Right c

type ChModule = CsimpleModule

type CsimpleModule = Cbody

type Cbody = Ctopdecls

type Ctopdecls = CBlock Ctopdecl

type Ctopdecl = Cdecl

type Cdecls = CBlock Cdecl

data Cdecl
  = CdeclGendecl Cgendecl
  | CdeclPatdecl Cpatdecl
  | CdeclFundecl Cfundecl
  deriving (Show, Eq, Ord)

wrapCdecl :: Either (Either Cgendecl Cpatdecl) Cfundecl -> Cdecl
wrapCdecl = either (either CdeclGendecl CdeclPatdecl) CdeclFundecl

unwrapCdecl :: Cdecl -> Either (Either Cgendecl Cpatdecl) Cfundecl
unwrapCdecl (CdeclGendecl c) = Left (Left c)
unwrapCdecl (CdeclPatdecl c) = Left (Right c)
unwrapCdecl (CdeclFundecl c) = Right c

data Cgendecl
  = CgendeclTypesigdecl Ctypesigdecl
  | CgendeclFixitydecl Cfixitydecl
  deriving (Show, Eq, Ord)

wrapCgendecl :: Either Ctypesigdecl Cfixitydecl -> Cgendecl
wrapCgendecl = either CgendeclTypesigdecl CgendeclFixitydecl

unwrapCgendecl :: Cgendecl -> Either Ctypesigdecl Cfixitydecl
unwrapCgendecl (CgendeclTypesigdecl c) = Left c
unwrapCgendecl (CgendeclFixitydecl c) = Right c

type Cpatdecl = (Cpat, Crhs)

type Cfundecl = (Cfunlhs, Crhs)

type Cops = [(Maybe Cop, Maybe CToken)]

type Cvars = [(Maybe Cvar, Maybe CToken)]

data Cfixity
  = CfixityLeft CToken
  | CfixityRight CToken
  | CfixityNone CToken
  deriving (Show, Eq, Ord)

wrapCfixity :: Either (Either CToken CToken) CToken -> Cfixity
wrapCfixity = either (either CfixityLeft CfixityRight) CfixityNone

unwrapCfixity :: Cfixity -> Either (Either CToken CToken) CToken
unwrapCfixity (CfixityLeft c) = Left (Left c)
unwrapCfixity (CfixityRight c) = Left (Right c)
unwrapCfixity (CfixityNone c) = Right c

type Cfixitydecl = (Cfixity, Cops)

type Ctypesigdecl = ((Cvars, CToken), (Cfullcontext, ChType))

type ChType = [(Maybe Cbtype, Maybe CToken)]

type Cbtype = [Maybe Catype]

data Catype
  = CatypeGtycon Cgtycon
  | CatypeTyVar CToken
  | CatypeTuple (CParens [(Maybe ChType, Maybe CToken)])
  | CatypeList (CBrackets ChType)
  | CatypeParen (CParens ChType)
  deriving (Show, Eq, Ord)

wrapCatype :: Either (Either (Either (Either Cgtycon CToken) (CParens [(Maybe ChType, Maybe CToken)])) (CBrackets ChType)) (CParens ChType) -> Catype
wrapCatype = either (either (either (either CatypeGtycon CatypeTyVar) CatypeTuple) CatypeList) CatypeParen

unwrapCatype :: Catype -> Either (Either (Either (Either Cgtycon CToken) (CParens [(Maybe ChType, Maybe CToken)])) (CBrackets ChType)) (CParens ChType)
unwrapCatype (CatypeGtycon c) = Left (Left (Left (Left c)))
unwrapCatype (CatypeTyVar c) = Left (Left (Left (Right c)))
unwrapCatype (CatypeTuple c) = Left (Left (Right c))
unwrapCatype (CatypeList c) = Left (Right c)
unwrapCatype (CatypeParen c) = Right c

data Cgtycon
  = CgtyconQtycon CToken
  | CgtyconUnitCon CunitCon
  | CgtyconListCon ClistCon
  | CgtyconFunCon CfunCon
  | CgtyconTupleCon CtupleCon
  deriving (Show, Eq, Ord)

wrapCgtycon :: Either (Either (Either (Either CToken CunitCon) ClistCon) CfunCon) CtupleCon -> Cgtycon
wrapCgtycon = either (either (either (either CgtyconQtycon CgtyconUnitCon) CgtyconListCon) CgtyconFunCon) CgtyconTupleCon

unwrapCgtycon :: Cgtycon -> Either (Either (Either (Either CToken CunitCon) ClistCon) CfunCon) CtupleCon
unwrapCgtycon (CgtyconQtycon c) = Left (Left (Left (Left c)))
unwrapCgtycon (CgtyconUnitCon c) = Left (Left (Left (Right c)))
unwrapCgtycon (CgtyconListCon c) = Left (Left (Right c))
unwrapCgtycon (CgtyconFunCon c) = Left (Right c)
unwrapCgtycon (CgtyconTupleCon c) = Right c

type CfunCon = CParens CToken

data Ccontext
  = CcontextOne ChClass
  | CcontextMany (CParens [(Maybe ChClass, Maybe CToken)])
  deriving (Show, Eq, Ord)

wrapCcontext :: Either ChClass (CParens [(Maybe ChClass, Maybe CToken)]) -> Ccontext
wrapCcontext = either CcontextOne CcontextMany

unwrapCcontext :: Ccontext -> Either ChClass (CParens [(Maybe ChClass, Maybe CToken)])
unwrapCcontext (CcontextOne c) = Left c
unwrapCcontext (CcontextMany c) = Right c

data Cfullcontext
  = CfullcontextFilled (Ccontext, CToken)
  | CfullcontextEmpty
  deriving (Show, Eq, Ord)

wrapCfullcontext :: Either (Ccontext, CToken) b -> Cfullcontext
wrapCfullcontext = either CfullcontextFilled (const CfullcontextEmpty)

unwrapCfullcontext :: Cfullcontext -> Either (Ccontext, CToken) ()
unwrapCfullcontext (CfullcontextFilled c) = Left c
unwrapCfullcontext CfullcontextEmpty = Right ()

type ChClass = Either ChClassSingle ChClassMultiple

type ChClassSingle = (CToken, CToken)

type ChClassMultiple = (((CToken, CToken), (CToken, [Maybe Catype])), CToken)

type Cptycls = CToken

type Cptyvar = CToken

type Cfunlhs = (Cvar, [Maybe Capat])

type Crhs = ((CToken, Cexp), CwhereDecls)

-- type CwhereDecls = (Maybe (CToken, Cdecls))
-- type CwhereDecls = Either (CToken, Cdecls) ()
type CwhereDecls = Either () (CToken, Cdecls)

type Calts = CBlock Calt

type Calt = Either Calt1 Calt2

type Calt1 = (((Cpat, CToken), Cexp), CwhereDecls)

type Calt2 = ((Cpat, Cgdpat), CwhereDecls)

type Cgdpat = [Maybe (((CToken, Cexp), CToken), Cexp)]