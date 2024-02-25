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
optimize t = fst <$> optimize' t 0

optimize' :: MonadFD4 m => Term -> Int -> m (Term, Bool)
optimize' t d = do
  (t', changed) <- (constantFolding <||> constantPropagation <||> inlineExpansion <||> recursiveOptimize d) t
  if changed
    then do
      (t'', _) <- optimize' t' d
      return (t'', True)
    else return (t', False)

recursiveOptimize :: MonadFD4 m => Int -> Term -> m (Term, Bool)
recursiveOptimize d (Lam p n ty t) = do
  let n' = n ++ show d
  (t', tChanged) <- optimize' (open n' t) (d + 1)
  return (Lam p n ty (close n' t'), tChanged)
recursiveOptimize d (App p u t) = do
  (u', uChanged) <- optimize' u (d + 1)
  (t', tChanged) <- optimize' t (d + 1)
  return (App p u' t', uChanged || tChanged)
recursiveOptimize d (Print p s t) = do
  (t', tChanged) <- optimize' t (d + 1)
  return (Print p s t', tChanged)
recursiveOptimize d (Fix p f fty x xty t) = do
  let x' = x ++ show d
  let f' = f ++ show d
  (t', tChanged) <- optimize' (openN [x', f'] t) (d + 1)
  return (Fix p f fty x xty (closeN [x', f'] t'), tChanged)
recursiveOptimize d (IfZ p c t e) = do
  (c', cChanged) <- optimize' c (d + 1)
  (t', tChanged) <- optimize' t (d + 1)
  (e', eChanged) <- optimize' e (d + 1)
  return (IfZ p c' t' e', cChanged || tChanged || eChanged)
recursiveOptimize d (Let p n ty e t) = do
  let n' = n ++ show d
  (e', eChanged) <- optimize' e (d + 1)
  (t', tChanged) <- optimize' (open n' t) (d + 1)
  return (Let p n ty e' (close n' t'), eChanged || tChanged)
recursiveOptimize d (BinaryOp p op t u) = do
  (t', tChanged) <- optimize' t (d + 1)
  (u', uChanged) <- optimize' u (d + 1)
  return (BinaryOp p op t' u', tChanged || uChanged)
recursiveOptimize _ t@(Const _ _) = return (t, False)
recursiveOptimize _ t@(V _ _) = return (t, False)

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
