{-# LANGUAGE TupleSections #-}

module Optimize where

import Eval (semOp)
import Lang
import MonadFD4
import Subst (subst)

(<.>) :: MonadFD4 m => (Term -> Term) -> (Term -> m (Term, Bool)) -> (Term -> m (Term, Bool))
(f <.> g) term = do
  (optimized, changed) <- g term
  return (f optimized, changed)

(<||>) :: MonadFD4 m => (Term -> m (Term, Bool)) -> (Term -> m (Term, Bool)) -> (Term -> m (Term, Bool))
(f <||> g) term = do
  (fterm, fchanged) <- f term
  (gfterm, gchanged) <- g fterm
  return (gfterm, fchanged || gchanged)

optimizeDecl :: MonadFD4 m => DeclTerm -> m DeclTerm
optimizeDecl (Decl p n ty t) = Decl p n ty <$> optimize t

optimize :: MonadFD4 m => Term -> m Term
optimize t = fst <$> optimize' t

optimize' :: MonadFD4 m => Term -> m (Term, Bool)
optimize' t = do
  (t', changed) <- (constantFolding <||> constantPropagation <||> inlineExpansion <||> recursiveOptimize) t
  if changed
    then do
      (t'', _) <- optimize' t'
      return (t'', True)
    else return (t', False)

recursiveOptimize :: MonadFD4 m => Term -> m (Term, Bool)
recursiveOptimize (Lam p n ty t) = (Lam p n ty <.> optimize') t
recursiveOptimize (App p u t) = do
  (u', uChanged) <- optimize' u
  (t', tChanged) <- optimize' t
  return (App p u' t', uChanged || tChanged)
recursiveOptimize (Print p s t) = (Print p s <.> optimize') t
recursiveOptimize (Fix p f fty x xty t) = (Fix p f fty x xty <.> optimize') t
recursiveOptimize (IfZ p c t e) = do
  (c', cChanged) <- optimize' c
  (t', tChanged) <- optimize' t
  (e', eChanged) <- optimize' e
  return (IfZ p c' t' e', cChanged || tChanged || eChanged)
recursiveOptimize (Let p n ty e t) = do
  (e', eChanged) <- optimize' e
  (t', tChanged) <- optimize' t
  return (Let p n ty e' t', eChanged || tChanged)
recursiveOptimize (BinaryOp p op t u) = do
  (t', tChanged) <- optimize' t
  (u', uChanged) <- optimize' u
  return (BinaryOp p op t' u', tChanged || uChanged)
recursiveOptimize t@(Const _ _) = return (t, False)
recursiveOptimize t@(V _ _) = return (t, False)

-- Optimizaciones
constantFolding :: MonadFD4 m => Term -> m (Term, Bool)
constantFolding (BinaryOp _ Add (Const _ (CNat 0)) u) = return (u, True)
constantFolding (BinaryOp _ _ t (Const _ (CNat 0))) = return (t, True)
constantFolding (BinaryOp p op (Const _ (CNat n)) (Const _ (CNat m))) = return (Const p (CNat (semOp op n m)), True)
constantFolding (IfZ _ (Const _ (CNat 0)) t _) = return (t, True)
constantFolding (IfZ _ (Const _ (CNat _)) _ e) = return (e, True)
constantFolding term = return (term, False)

constantPropagation :: MonadFD4 m => Term -> m (Term, Bool)
constantPropagation (Let _ _ _ c@(Const _ _) t) = return (subst c t, True)
constantPropagation term = return (term, False)

inlineExpansion :: MonadFD4 m => Term -> m (Term, Bool)
inlineExpansion (Let _ _ _ e t) = return (subst e t, True)
inlineExpansion (App _ (Lam _ _ _ b) t) = return (subst t b, True)
inlineExpansion term = return (term, False)
