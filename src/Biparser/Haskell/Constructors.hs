{-# LANGUAGE RankNTypes #-}

module Biparser.Haskell.Constructors where

import Biparser
import Control.Lens hiding (iso)
import Language.Haskell.AST
import PErr

{-
Partial isomorphisms for the various constructors of the AST, in accordance with the approach discussed in Biparser: Exact Printing for Data Synchronization. Ideally, these would be generated automatically.
-}

to3 :: Biparser () ((a, b), c) (a, b, c)
to3 = iso (\((a, b), c) -> (a, b, c)) (\(a, b, c) -> ((a, b), c))

to4 :: Biparser () (((a, b), c), d) (a, b, c, d)
to4 = iso (\(((a, b), c), d) -> (a, b, c, d)) (\(a, b, c, d) -> (((a, b), c), d))

mk3 :: String -> Prism' d (a, b, c) -> Biparser () ((a, b), c) d
mk3 cName p = to3 >#>> mk cName p

mk4 :: String -> Prism' e (a, b, c, d) -> Biparser () (((a, b), c), d) e
mk4 cName p = to4 >#>> mk cName p

{- HsModule -}

mkHsModule :: Biparser () (((Module, Maybe [HsExportSpec]), [HsImportDecl]), [HsDecl]) HsModule
mkHsModule = mk4 "HsModule" _HsModule

{- HsLiteral -}

mkHsFloat :: Biparser () String HsLiteral
mkHsFloat = mk "HsFloat" _HsFloat

mkHsInteger :: Biparser () String HsLiteral
mkHsInteger = mk "HsInteger" _HsInteger

mkHsChar :: Biparser () Char HsLiteral
mkHsChar = mk "HsChar" _HsChar

mkHsString :: Biparser () String HsLiteral
mkHsString = mk "HsString" _HsString

{- HsExp -}

mkHsVar :: Biparser () HsQName HsExp
mkHsVar = mk "HsVar" _HsVar

mkHsCon :: Biparser () HsQName HsExp
mkHsCon = mk "HsCon" _HsCon

mkHsLit :: Biparser () HsLiteral HsExp
mkHsLit = mk "HsLit" _HsLit

mkHsApp :: Biparser () (HsExp, HsExp) HsExp
mkHsApp = mk "HsApp" _HsApp

-- Special custom version of mkHsApp which accepts a list instead.
mkHsApp' :: Biparser () [HsExp] HsExp
mkHsApp' = partialIso f g
  where
    f [] = Left $ Error "mkHsApp'.parse: got empty list"
    f (e : es) = Right $ foldl HsApp e es
    g (HsApp e1 e2) = case e2 of
      HsApp _ _ -> Left $ Error "mkHsApp'.print: HsApp's association was wrong"
      _ -> (++ [e2]) <$> g e1
    g e = Right [e]

-- >>> testParse mkHsApp' [HsLit (HsInteger "1"),HsLit (HsInteger "2"),HsLit (HsInteger "3")] ""
-- Right ((HsApp (HsApp (HsLit (HsInteger "1")) (HsLit (HsInteger "2"))) (HsLit (HsInteger "3")),()),("",[],Ln 1, Col 1))

-- >>> testPrint mkHsApp' (HsApp (HsApp (HsLit (HsInteger "1")) (HsLit (HsInteger "2"))) (HsLit (HsInteger "3")), Nothing)
-- Right ([HsLit (HsInteger "1"),HsLit (HsInteger "2"),HsLit (HsInteger "3")],("",[],Ln 1, Col 1))

-- NOTE: mkHsApp' should fail if the given HsExp contains 'wrong'-associated HsApp's! Same with the other ' variants.

-- >>> testPrint mkHsApp' (HsApp (HsLit (HsInteger "1")) (HsApp (HsLit (HsInteger "2")) (HsLit (HsInteger "3"))), Nothing)
-- Left mkHsApp'.print: HsApp's association was wrong

mkHsInfixApp :: Biparser () ((HsExp, HsQOp), HsExp) HsExp
mkHsInfixApp = mk3 "HsInfixApp" _HsInfixApp

mkHsCase :: Biparser () (HsExp, [HsAlt]) HsExp
mkHsCase = mk "HsCase" _HsCase

mkHsIf :: Biparser () ((HsExp, HsExp), HsExp) HsExp
mkHsIf = mk3 "HsIf" _HsIf

mkHsNegApp :: Biparser () HsExp HsExp
mkHsNegApp = mk "HsNegApp" _HsNegApp

mkHsLambda :: Biparser () ([HsPat], HsExp) HsExp
mkHsLambda = mk "HsLambda" _HsLambda

mkHsLet :: Biparser () ([HsDecl], HsExp) HsExp
mkHsLet = mk "HsLet" _HsLet

mkHsList :: Biparser () [HsExp] HsExp
mkHsList = mk "HsList" _HsList

mkHsParen :: Biparser () HsExp HsExp
mkHsParen = mk "HsParen" _HsParen

mkHsLeftSection :: Biparser () (HsExp, HsQOp) HsExp
mkHsLeftSection = mk "HsLeftSection" _HsLeftSection

mkHsRightSection :: Biparser () (HsQOp, HsExp) HsExp
mkHsRightSection = mk "HsRightSection" _HsRightSection

{- HsDecl -}

mkHsFunBind :: Biparser () (((HsName, [HsPat]), HsRhs), HsWhere) HsDecl
mkHsFunBind = mk4 "HsFunBind" _HsFunBind

mkHsPatBind :: Biparser () ((HsPat, HsRhs), HsWhere) HsDecl
mkHsPatBind = mk3 "HsPatBind" _HsPatBind

mkHsTypeSig :: Biparser () ([HsName], HsQualType) HsDecl
mkHsTypeSig = mk "HsTypeSig" _HsTypeSig

mkHsInfixDecl :: Biparser () ((HsAssoc, Int), [HsOp]) HsDecl
mkHsInfixDecl = mk3 "HsInfixDecl" _HsInfixDecl

{- HsAssoc -}

mkHsAssocNone :: Biparser () () HsAssoc
mkHsAssocNone = mk "HsAssocNone" _HsAssocNone

mkHsAssocLeft :: Biparser () () HsAssoc
mkHsAssocLeft = mk "HsAssocLeft" _HsAssocLeft

mkHsAssocRight :: Biparser () () HsAssoc
mkHsAssocRight = mk "HsAssocRight" _HsAssocRight

{- HsRhs -}

mkHsUnguardedRhs :: Biparser () HsExp HsRhs
mkHsUnguardedRhs = mk "HsUnguardedRhs" _HsUnguardedRhs

{- HsPat -}

mkHsPVar :: Biparser () HsName HsPat
mkHsPVar = mk "HsPVar" _HsPVar

mkHsPLit :: Biparser () HsLiteral HsPat
mkHsPLit = mk "HsPLit" _HsPLit

mkHsPApp :: Biparser () (HsQName, [HsPat]) HsPat
mkHsPApp = mk "HsPApp" _HsPApp

mkHsPInfixApp :: Biparser () ((HsPat, HsQOp), HsPat) HsPat
mkHsPInfixApp = mk3 "HsPInfixApp" _HsPInfixApp

mkHsPParen :: Biparser () HsPat HsPat
mkHsPParen = mk "HsPParen" _HsPParen

mkHsPWildCard :: Biparser () () HsPat
mkHsPWildCard = mk "HsPWildCard" _HsPWildCard

{- HsAlt -}

mkHsAlt :: Biparser () ((HsPat, HsGuardedAlts), HsWhere) HsAlt
mkHsAlt = mk3 "HsAlt" _HsAlt

{- HsGuardedAlts-}

mkHsUnguardedAlt :: Biparser () HsExp HsGuardedAlts
mkHsUnguardedAlt = mk "HsUnguardedAlt" _HsUnguardedAlt

mkHsGuardedAlts :: Biparser () [HsGuardedAlt] HsGuardedAlts
mkHsGuardedAlts = mk "HsGuardedAlts" _HsGuardedAlts

{- HsGuardedAlt-}

mkHsGuardedAlt :: Biparser () (HsExp, HsExp) HsGuardedAlt
mkHsGuardedAlt = mk "HsGuardedAlt" _HsGuardedAlt

{- Module -}

mkModule :: Biparser () String Module
mkModule = mk "Module" _Module

{- HsQName -}

mkQual :: Biparser () (Module, HsName) HsQName
mkQual = mk "Qual" _Qual

mkUnQual :: Biparser () HsName HsQName
mkUnQual = mk "UnQual" _UnQual

mkSpecial :: Biparser () HsSpecialCon HsQName
mkSpecial = mk "Special" _Special

{- HsName -}

mkHsIdent :: Biparser () String HsName
mkHsIdent = mk "HsIdent" _HsIdent

mkHsSymbol :: Biparser () String HsName
mkHsSymbol = mk "HsSymbol" _HsSymbol

{- HsQOp -}

mkHsQVarOp :: Biparser () HsQName HsQOp
mkHsQVarOp = mk "HsQVarOp" _HsQVarOp

mkHsQConOp :: Biparser () HsQName HsQOp
mkHsQConOp = mk "HsQConOp" _HsQConOp

{- HsOp-}

mkHsVarOp :: Biparser () HsName HsOp
mkHsVarOp = mk "HsVarOp" _HsVarOp

mkHsConOp :: Biparser () HsName HsOp
mkHsConOp = mk "HsConOp" _HsConOp

{- HsSpecialCon -}

mkHsUnitCon :: Biparser () () HsSpecialCon
mkHsUnitCon = mk "HsUnitCon" _HsUnitCon

mkHsListCon :: Biparser () () HsSpecialCon
mkHsListCon = mk "HsListCon" _HsListCon

mkHsFunCon :: Biparser () () HsSpecialCon
mkHsFunCon = mk "HsFunCon" _HsFunCon

mkHsTupleCon :: Biparser () Int HsSpecialCon
mkHsTupleCon = mk "HsTupleCon" _HsTupleCon

mkHsCons :: Biparser () () HsSpecialCon
mkHsCons = mk "HsCons" _HsCons

{- HsQualType -}

mkHsQualType :: Biparser () (HsContext, HsType) HsQualType
mkHsQualType = mk "HsQualType" _HsQualType

{- HsType -}

mkHsTyFun :: Biparser () (HsType, HsType) HsType
mkHsTyFun = mk "HsTyFun" _HsTyFun

-- Special version of mkHsTyFun which accepts a list instead.
mkHsTyFun' :: Biparser () [HsType] HsType
mkHsTyFun' = partialIso f g
  where
    f [] = Left $ Error "mkHsTyFun'.parse: got empty list"
    f [t] = Right t
    f (t : ts) = HsTyFun t <$> f ts
    g (HsTyFun t1 t2) = case t1 of
      HsTyFun _ _ -> Left $ Error "mkHsTyFun'.print: HsTyFun's association was wrong "
      _ -> (t1 :) <$> g t2
    g t = Right [t]

mkHsTyTuple :: Biparser () [HsType] HsType
mkHsTyTuple = mk "HsTyTuple" _HsTyTuple

mkHsTyApp :: Biparser () (HsType, HsType) HsType
mkHsTyApp = mk "HsTyApp" _HsTyApp

-- Special version of mkHsTyApp which accepts a list instead.
mkHsTyApp' :: Biparser () [HsType] HsType
mkHsTyApp' = partialIso f g
  where
    f [] = Left $ Error "mkHsTyApp'.parse: got empty list"
    f (t : ts) = Right $ foldl HsTyApp t ts
    g (HsTyApp t1 t2) = case t2 of
      HsTyApp _ _ -> Left $ Error "mkHsTyApp'.print: HsTyApp's association was wrong"
      _ -> (++ [t2]) <$> g t1
    g t = Right [t]

mkHsTyAppList :: Biparser () HsType HsType
mkHsTyAppList = mk "HsTyAppList" _HsTyAppList

mkHsTyVar :: Biparser () HsName HsType
mkHsTyVar = mk "HsTyVar" _HsTyVar

mkHsTyCon :: Biparser () HsQName HsType
mkHsTyCon = mk "HsTyCon" _HsTyCon

mkHsTyParen :: Biparser () HsType HsType
mkHsTyParen = mk "HsTyParen" _HsTyParen
