{-# LANGUAGE TemplateHaskell #-}

module Language.Haskell.AST where

import Control.Lens

-- The data types are heavily inspired by those from haskell-src:
-- https://hackage.haskell.org/package/haskell-src-1.0.4.1/docs/Language-Haskell-Syntax.html
-- There are minor differences, such as not storing source locations.
-- There are also larger differences to better fit biparsers.

data HsModule = HsModule Module (Maybe [HsExportSpec]) [HsImportDecl] [HsDecl]
  deriving (Show, Ord, Eq)

newtype HsExportSpec
  = HsEVar HsQName
  deriving (Show, Ord, Eq)

data HsImportDecl = HsImportDecl
  { importModule :: Module,
    importQualified :: Bool,
    importAs :: Maybe Module,
    importSpecs :: Maybe (Bool, [HsImportSpec])
  }
  deriving (Show, Ord, Eq)

newtype HsImportSpec
  = HsIVar HsName
  deriving (Show, Ord, Eq)

data HsLiteral
  = HsFloat String
  | HsInteger String
  | HsChar Char
  | HsString String
  deriving (Show, Ord, Eq)

data HsExp
  = HsVar HsQName
  | HsCon HsQName
  | HsLit HsLiteral
  | HsApp HsExp HsExp
  | HsInfixApp HsExp HsQOp HsExp
  | HsCase HsExp [HsAlt]
  | HsIf HsExp HsExp HsExp
  | HsNegApp HsExp
  | HsLambda [HsPat] HsExp
  | HsLet [HsDecl] HsExp
  | HsList [HsExp]
  | HsParen HsExp
  | HsLeftSection HsExp HsQOp
  | HsRightSection HsQOp HsExp
  deriving (Show, Ord, Eq)

type HsWhere = [HsDecl]

data HsDecl
  = HsFunBind HsName [HsPat] HsRhs HsWhere
  | HsPatBind HsPat HsRhs HsWhere
  | HsTypeSig [HsName] HsQualType
  | HsInfixDecl HsAssoc Int [HsOp]
  deriving (Show, Ord, Eq)

data HsAssoc
  = HsAssocNone
  | HsAssocLeft
  | HsAssocRight
  deriving (Show, Eq, Ord)

data HsRhs
  = HsUnguardedRhs HsExp
  | HsGuardedRhss [HsGuardedRhs]
  deriving (Show, Ord, Eq)

data HsGuardedRhs
  = HsGuardedRhs HsExp HsExp
  deriving (Show, Ord, Eq)

data HsPat
  = HsPVar HsName
  | HsPLit HsLiteral
  | HsPApp HsQName [HsPat]
  | HsPInfixApp HsPat HsQOp HsPat
  | HsPParen HsPat
  | HsPWildCard
  deriving (Show, Ord, Eq)

data HsAlt = HsAlt HsPat HsGuardedAlts HsWhere
  deriving (Show, Ord, Eq)

data HsGuardedAlts
  = HsUnguardedAlt HsExp -- -> exp
  | HsGuardedAlts [HsGuardedAlt] -- gdpat
  deriving (Show, Ord, Eq)

data HsGuardedAlt = HsGuardedAlt HsExp HsExp -- "| exp -> exp"
  deriving (Show, Ord, Eq)

newtype Module = Module String
  deriving (Show, Ord, Eq)

data HsQName
  = Qual Module HsName
  | UnQual HsName
  | Special HsSpecialCon
  deriving (Show, Ord, Eq)

data HsName
  = HsIdent String -- varid or conid
  | HsSymbol String -- varsym or consym
  deriving (Show, Ord, Eq)

data HsQOp
  = HsQVarOp HsQName -- qvarop
  | HsQConOp HsQName -- qconop
  deriving (Show, Ord, Eq)

isNegate :: HsQOp -> Bool
isNegate (HsQVarOp (Qual _ (HsSymbol "-"))) = True
isNegate (HsQVarOp (UnQual (HsSymbol "-"))) = True
isNegate _ = False

data HsOp
  = HsVarOp HsName -- varop
  | HsConOp HsName -- conop
  deriving (Show, Ord, Eq)

data HsSpecialCon
  = HsUnitCon -- ()
  | HsListCon -- []
  | HsFunCon -- ->
  | HsTupleCon Int -- (,{,}) with arity
  | HsCons -- :
  deriving (Show, Ord, Eq)

data HsQualType
  = HsQualType HsContext HsType
  deriving (Show, Ord, Eq)

type HsContext = [HsAsst]

-- | Class assertion.
type HsAsst = (HsQName, [HsType])

data HsType
  = HsTyFun HsType HsType -- a -> b
  | HsTyTuple [HsType] -- (a,b,c)
  | HsTyApp HsType HsType -- Maybe a  |  f a
  | HsTyAppList HsType -- [] a | [a]
  | HsTyVar HsName -- a
  | HsTyCon HsQName -- Maybe  |  f
  | HsTyParen HsType -- (t)
  deriving (Show, Ord, Eq)

makePrisms ''HsModule
makePrisms ''HsExportSpec
makePrisms ''HsImportDecl
makePrisms ''HsImportSpec
makePrisms ''HsLiteral
makePrisms ''HsExp
makePrisms ''HsDecl
makePrisms ''HsAssoc
makePrisms ''HsRhs
makePrisms ''HsPat
makePrisms ''HsAlt
makePrisms ''HsGuardedAlts
makePrisms ''HsGuardedAlt
makePrisms ''Module
makePrisms ''HsQName
makePrisms ''HsName
makePrisms ''HsQOp
makePrisms ''HsOp
makePrisms ''HsSpecialCon
makePrisms ''HsQualType
makePrisms ''HsType
