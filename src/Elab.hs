{-|
Module      : Elab
Description : Elabora un término fully named a uno locally closed.
Copyright   : (c) Mauro Jaskelioff, Guido Martínez, 2020.
License     : GPL-3
Maintainer  : mauro@fceia.unr.edu.ar
Stability   : experimental

Este módulo permite elaborar términos y declaraciones para convertirlas desde
fully named (@NTerm) a locally closed (@Term@)
-}

module Elab ( elab, desugar, desugarType ) where

import           Lang
import           MonadFD4
import           Subst


desugar :: MonadFD4 m => SNTerm -> m NTerm
desugar (SV info var) = V info var
desugar (SConst info c) = Const info c
desugar (SApp info sterm1 sterm2) = App info (desugar sterm1) (desugar sterm2)
desugar (SPrint info string sterm) = Print info string (desugar sterm)
desugar (SBinaryOp info op sterm1 sterm2) = BinaryOp info op (desugar sterm1) (desugar sterm2)
desugar (SIfZ info cond true false) = IfZ info (desugar cond) (desugar true) (desugar false)
desugar (SLam info [] body) = desugar body
desugar (SLam info ((bname,bty):bs) body) = Lam info bname (desugarType bty) $ desugar (SLam info bs body)
desugar (SFix info name1 type1 name2 type2 body) = Fix info name1 (desugarType type1) name2 (desugarType type2) (desugar body)
desugar (SLet info name ty sterm1 sterm2) = Let info name (desugarType ty) (desugar sterm1) (desugar sterm2)

-- todo: fix STySyn case
desugarType :: MonadFD4 m => STy -> m Ty
desugarType SNatTy           = NatTy
desugarType (SFunTy ty1 ty2) = FunTy (desugarType ty1) (desugarType ty2)
desugarType (STySyn ty)      = do
  ty <- lookupTy name
  case ty of
    Just ty' -> return ty'
    Nothing  -> failFD4 $ "unknown type synonym for " ++ show ty -- todo: should fail with pos? pass

-- | 'elab' transforma variables ligadas en índices de de Bruijn
-- en un término dado.
elab :: SNTerm -> Term
elab = elab' [] . desugar

elab' :: [Name] -> NTerm -> Term
elab' env (V p v) =
  -- Tenemos que ver si la variable es Global o es un nombre local
  -- En env llevamos la lista de nombres locales.
  if v `elem` env
    then V p (Free v)
    else V p (Global v)

elab' _ (Const p c) = Const p c
elab' env (Lam p v ty t) = Lam p v ty (close v (elab' (v:env) t))
elab' env (Fix p f fty x xty t) = Fix p f fty x xty (closeN [f, x] (elab' (x:f:env) t))
elab' env (IfZ p c t e)         = IfZ p (elab' env c) (elab' env t) (elab' env e)
elab' env (Print i str t) = Print i str (elab' env t)
elab' env (BinaryOp i o t u) = BinaryOp i o (elab' env t) (elab' env u)
elab' env (App p h a) = App p (elab' env h) (elab' env a)
elab' env (Let p v vty def body) = Let p v vty (elab' env def) (close v (elab' (v:env) body))

elabDecl :: SNTermDecl -> TermDecl
elabDecl (Decl pos name sty snterm) = Decl pos name (desugarType sty) (elab snterm)
elabDecl (DeclTy pos name sty) = DeclTy pos name (desugarType sty)
