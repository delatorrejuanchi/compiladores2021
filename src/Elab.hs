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
desugar (SV info var) = return $ V info var
desugar (SConst info c) = return $ Const info c
desugar (SApp info sterm1 sterm2) = do
  term1 <- desugar sterm1
  term2 <- desugar sterm2
  return $ App info term1 term2
desugar (SPrint info string sterm) = Print info string <$> desugar sterm
desugar (SBinaryOp info op sterm1 sterm2) = do
  term1 <- desugar sterm1
  term2 <- desugar sterm1
  return $ BinaryOp info op term1 term2
desugar (SIfZ info cond true false) = do
  cond' <- desugar cond
  true' <- desugar true
  false' <- desugar false
  return $ IfZ info cond' true' false'
desugar (SLam info [] body) = desugar body
desugar (SLam info ((bname,bty):bs) body) = do
  ty <- desugarType bty
  Lam info bname ty <$> desugar (SLam info bs body)
desugar (SFix info name1 type1 name2 type2 body) = do
  ty1 <- desugarType type1
  ty2 <- desugarType type2
  Fix info name1 ty1 name2 ty2 <$> desugar body
desugar (SLet info False name sty sterm1 sterm2) = do
  ty <- desugarType sty
  term1 <- desugar sterm1
  term2 <- desugar sterm2
  return $ Let info name ty term1 term2
desugar (SLet info True name sty sterm1 sterm2) = undefined -- TODO: implement

-- todo: fix STySyn case
desugarType :: MonadFD4 m => STy -> m Ty
desugarType SNatTy           = return NatTy
desugarType (SFunTy sty1 sty2) = do
  ty1 <- desugarType sty1
  ty2 <- desugarType sty2
  return $ FunTy ty1 ty2
desugarType (STySyn name)      = do
  ty <- lookupTy name
  case ty of
    Just ty' -> return ty'
    Nothing  -> failFD4 $ "unknown type synonym for " ++ show ty -- todo: should fail with pos? pass

-- | 'elab' transforma variables ligadas en índices de de Bruijn
-- en un término dado.
elab :: MonadFD4 m => SNTerm -> m Term
elab t = elab' [] <$> desugar t

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

elabDecl :: MonadFD4 m => SNTermDecl -> m TermDecl
elabDecl (Decl pos name sty snterm) = do
  ty <- desugarType sty
  Decl pos name ty <$> elab snterm
elabDecl (DeclTy pos name sty) = DeclTy pos name <$> desugarType sty
