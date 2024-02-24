{-# LANGUAGE TupleSections #-}

module Optimize where

import Eval (semOp)
import Lang
import MonadFD4
import Subst

(<.>) :: MonadFD4 m => (Term -> Term) -> (Term -> m (Term, Bool)) -> (Term -> m (Term, Bool))
(f <.> g) term = do
  (optimized, changed) <- g term
  return (f optimized, changed)

(<||>) :: MonadFD4 m => (Term -> m (Term, Bool)) -> (Term -> m (Term, Bool)) -> (Term -> m (Term, Bool))
(f <||> g) term = do
  (fterm, fchanged) <- f term
  if fchanged
    then return (fterm, True)
    else g term

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
recursiveOptimize (Lam p n ty t) = do
  (t', tChanged) <- optimize' $ open n t
  return (Lam p n ty (close n t'), tChanged)
recursiveOptimize (App p u t) = do
  (u', uChanged) <- optimize' u
  (t', tChanged) <- optimize' t
  return (App p u' t', uChanged || tChanged)
recursiveOptimize (Print p s t) = (Print p s <.> optimize') t
recursiveOptimize (Fix p f fty x xty t) = do
  (t', tChanged) <- optimize' $ openN [x, f] t
  return (Fix p f fty x xty (closeN [x, f] t'), tChanged)
recursiveOptimize (IfZ p c t e) = do
  (c', cChanged) <- optimize' c
  (t', tChanged) <- optimize' t
  (e', eChanged) <- optimize' e
  return (IfZ p c' t' e', cChanged || tChanged || eChanged)
recursiveOptimize (Let p n ty e t) = do
  (e', eChanged) <- optimize' e
  (t', tChanged) <- optimize' $ open n t
  return (Let p n ty e' (close n t'), eChanged || tChanged)
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
