{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE TupleSections #-}

-- |
-- Module      : Lang
-- Description : AST de términos, declaraciones y tipos
-- Copyright   : (c) Mauro Jaskelioff, Guido Martínez, 2020.
-- License     : GPL-3
-- Maintainer  : mauro@fceia.unr.edu.ar
-- Stability   : experimental
--
-- Definiciones de distintos tipos de datos:
--   - AST de términos
--   - Declaraciones
--   - Tipos
--   - Variables
module Lang where

import Common (Pos)
import Data.List.Extra (nubSort)
import Data.List.NonEmpty (NonEmpty ((:|)), toList)

-- | AST de Tipos
data Ty
  = NatTy
  | FunTy Ty Ty
  deriving (Show, Eq)

type Name = String

newtype Const = CNat Int
  deriving (Show)

data BinaryOp = Add | Sub
  deriving (Show)

-- | tipo de datos de declaraciones
data Decl var ty = Decl {declPos :: Pos, declName :: Name, declType :: ty, declBody :: Tm Pos var ty}
  deriving (Show, Functor, Foldable, Traversable)

data SDecl
  = SDeclVar Pos Name STy SNTerm
  | SDeclFun Pos Name Binders STy SNTerm
  | SDeclRec Pos Name Binders STy SNTerm
  deriving (Show)

-- | AST de los términos.
--   - info es información extra que puede llevar cada nodo.
--       Por ahora solo la usamos para guardar posiciones en el código fuente.
--   - var es el tipo de la variables. Es 'Name' para fully named y 'Var' para locally closed.
data Tm info var ty
  = V info var
  | Const info Const
  | Lam info Name ty (Tm info var ty)
  | App info (Tm info var ty) (Tm info var ty)
  | Print info String (Tm info var ty)
  | BinaryOp info BinaryOp (Tm info var ty) (Tm info var ty)
  | Fix info Name ty Name ty (Tm info var ty)
  | IfZ info (Tm info var ty) (Tm info var ty) (Tm info var ty)
  | Let info Name ty (Tm info var ty) (Tm info var ty)
  deriving (Show, Functor, Foldable, Traversable)

data STy
  = SNatTy
  | SFunTy STy STy
  deriving (Show, Eq)

data SNTerm
  = SV Pos Name
  | SConst Pos Const
  | SApp Pos SNTerm SNTerm
  | SPrint Pos String SNTerm
  | SBinaryOp Pos BinaryOp SNTerm SNTerm
  | SFix Pos Name STy Name STy SNTerm
  | SIfZ Pos SNTerm SNTerm SNTerm
  | SLet Pos Name STy SNTerm SNTerm
  | SLam Pos Binders SNTerm
  | SPrintEta Pos String
  | SLetFun Pos Name Binders STy SNTerm SNTerm
  | SLetRec Pos Name Binders STy SNTerm SNTerm
  deriving (Show)

type Binders = NonEmpty (Name, STy)

type Multibinders = NonEmpty (NonEmpty Name, STy)

multibindersToBinders :: Multibinders -> Binders
multibindersToBinders ((x :| xs, xty) :| []) = (x, xty) :| map (,xty) xs
multibindersToBinders ((x :| xs, xty) :| b : bb) = (x, xty) :| map (,xty) xs <> toList (multibindersToBinders (b :| bb))

-- | 'Tm' con 'Name's como variables ligadas y libres y globales, guarda posición
type NTerm = Tm Pos Name Ty

-- | 'Decl' para NTerms
type DeclNTerm = Decl Name Ty

-- | 'Tm' con índices de De Bruijn como variables ligadas, y nombres para libres y globales, guarda posición
type Term = Tm Pos Var Ty

-- | 'Decl' para Terms
type DeclTerm = Decl Var Ty

data Var
  = Bound !Int
  | Free Name
  | Global Name
  deriving (Show)

-- | Obtiene la info en la raíz del término.
getInfo :: Tm info var ty -> info
getInfo (V i _) = i
getInfo (Const i _) = i
getInfo (Lam i _ _ _) = i
getInfo (App i _ _) = i
getInfo (Print i _ _) = i
getInfo (Fix i _ _ _ _ _) = i
getInfo (IfZ i _ _ _) = i
getInfo (Let i _ _ _ _) = i
getInfo (BinaryOp i _ _ _) = i

-- | Obtiene los nombres de variables (abiertas o globales) de un término.
freeVars :: Tm info Var ty -> [Name]
freeVars tm = nubSort $ go tm []
  where
    go (V _ (Free v)) xs = v : xs
    go (V _ (Global v)) xs = v : xs
    go (V _ _) xs = xs
    go (Lam _ _ _ t) xs = go t xs
    go (App _ l r) xs = go l $ go r xs
    go (Print _ _ t) xs = go t xs
    go (BinaryOp _ _ t u) xs = go t $ go u xs
    go (Fix _ _ _ _ _ t) xs = go t xs
    go (IfZ _ c t e) xs = go c $ go t $ go e xs
    go (Const _ _) xs = xs
    go (Let _ _ _ e t) xs = go e (go t xs)
