{-# LANGUAGE DeriveFunctor #-}

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

import           Common          (Pos)
import           Data.List.Extra (nubSort)

-- | AST de Tipos
data Ty =
      NatTy
    | FunTy Ty Ty
    deriving (Show,Eq)

-- | AST de Tipos azucarado.
data STy =
    SNatTy
  | SFunTy STy STy
  | STySyn Name
  deriving (Show, Eq)

type Name = String

-- | AST superficial de Tipos
data STy
  = SNatTy
  | SFunTy STy STy
  | STySyn Name
  deriving (Show, Eq)

data Const = CNat Int
  deriving (Show)

data BinaryOp = Add | Sub
  deriving (Show)

-- | tipo de datos de declaraciones, parametrizado por el tipo del cuerpo de la declaración
data Decl a b =
    Decl { declPos :: Pos, declName :: Name, declTy :: a, declBody :: b }
  | DeclTy { declPos :: Pos, declName :: Name, declTy :: a }
  deriving (Show, Functor)

type TermDecl = Decl Ty Term
type SNTermDecl = Decl STy SNTerm

-- | AST de los términos.
--   - info es información extra que puede llevar cada nodo.
--       Por ahora solo la usamos para guardar posiciones en el código fuente.
--   - var es el tipo de la variables. Es 'Name' para fully named y 'Var' para locally closed.
data Tm info var =
    V info var
  | Const info Const
  | Lam info Name Ty (Tm info var)
  | App info (Tm info var) (Tm info var)
  | Print info String (Tm info var)
  | BinaryOp info BinaryOp (Tm info var) (Tm info var)
  | Fix info Name Ty Name Ty (Tm info var)
  | IfZ info (Tm info var) (Tm info var) (Tm info var)
  | Let info Name Ty (Tm info var) (Tm info var)
  deriving (Show, Functor)

-- | AST superficial de los términos.
data STm info var
  = SV info var
  | SConst info Const
  | SLam info [(Name, STy)] (STm info var)
  | SApp info (STm info var) (STm info var)
  | SPrint info String (STm info var)
  | SPrintEta info String
  | SBinaryOp info BinaryOp (STm info var) (STm info var)
  | SFix info Name STy Name STy (STm info var)
  | -- | SFix info Name STy [(Name, STy)] (STm info var) multibinders
    SIfZ info (STm info var) (STm info var) (STm info var)
  | SLet info Bool Name STy (STm info var) (STm info var)
  deriving (Show, Functor)

-- | AST superficial.
-- todo: multibinders (should be as simple as changing [(Name, STy)]) to [([Name], STy)]
-- todo: should let have multiple declarations? not included in pdf
-- todo: parens in parser
data STm info var =
    SV info var
  | SConst info Const
  | SLam info [(Name, STy)] (STm info var)
  | SApp info (STm info var) (STm info var)
  | SPrint info String (STm info var)
  | SBinaryOp info BinaryOp (STm info var) (STm info var)
  | SFix info Name STy Name STy (STm info var) -- | SFix info Name STy [(Name, STy)] (STm info var) multibinders
  | SIfZ info (STm info var) (STm info var) (STm info var)
  | SLet info Name STy (STm info var) (STm info var)
  deriving (Show, Functor)

type SNTerm = STm Pos Name

data Var =
    Bound !Int
  | Free Name
  | Global Name
  deriving (Show)

-- | Obtiene la info en la raíz del término.
getInfo :: Tm info var -> info
getInfo (V i _)            = i
getInfo (Const i _)        = i
getInfo (Lam i _ _ _)      = i
getInfo (App i _ _)        = i
getInfo (Print i _ _)      = i
getInfo (Fix i _ _ _ _ _)  = i
getInfo (IfZ i _ _ _)      = i
getInfo (Let i _ _ _ _)    = i
getInfo (BinaryOp i _ _ _) = i

-- | Obtiene los nombres de variables (abiertas o globales) de un término.
freeVars :: Tm info Var -> [Name]
freeVars tm = nubSort $ go tm []
  where
    go (V _ (Free v)) xs     = v : xs
    go (V _ (Global v)) xs   = v : xs
    go (V _ _) xs            = xs
    go (Lam _ _ _ t) xs      = go t xs
    go (App _ l r) xs        = go l $ go r xs
    go (Print _ _ t) xs      = go t xs
    go (BinaryOp _ _ t u) xs = go t $ go u xs
    go (Fix _ _ _ _ _ t) xs  = go t xs
    go (IfZ _ c t e) xs      = go c $ go t $ go e xs
    go (Const _ _) xs        = xs
    go (Let _ _ _ e t) xs    = go e (go t xs)
