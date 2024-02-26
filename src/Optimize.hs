module Optimize where

import Eval (semOp)
import Lang
import MonadFD4
import Subst

type Optimizer m = Term -> m (Term, Bool)

(<||>) :: MonadFD4 m => Optimizer m -> Optimizer m -> Optimizer m
(f <||> g) term = do
  (fterm, fchanged) <- f term
  if fchanged
    then return (fterm, True)
    else g term

(<&&>) :: MonadFD4 m => Optimizer m -> Optimizer m -> Optimizer m
(f <&&> g) term = do
  (fterm, fchanged) <- f term
  if fchanged
    then g fterm
    else return (fterm, False)

optimizeDecl :: MonadFD4 m => DeclTerm -> m DeclTerm
optimizeDecl (Decl p n ty t) = Decl p n ty <$> optimize t

optimize :: MonadFD4 m => Term -> m Term
optimize t = fst <$> optimize' 0 t

optimize' :: MonadFD4 m => Int -> Optimizer m
optimize' d = (constantFolding <||> constantPropagation <||> inlineExpansion <||> recursiveOptimize d) <&&> optimize' d

recursiveOptimize :: MonadFD4 m => Int -> Optimizer m
recursiveOptimize d (Lam p n ty t) = do
  let n' = n ++ show d
  (t', tChanged) <- optimize' (d + 1) (open n' t)
  return (Lam p n ty (close n' t'), tChanged)
recursiveOptimize d (App p u t) = do
  (u', uChanged) <- optimize' (d + 1) u
  (t', tChanged) <- optimize' (d + 1) t
  return (App p u' t', uChanged || tChanged)
recursiveOptimize d (Print p s t) = do
  (t', tChanged) <- optimize' (d + 1) t
  return (Print p s t', tChanged)
recursiveOptimize d (Fix p f fty x xty t) = do
  let x' = x ++ show d
  let f' = f ++ show d
  (t', tChanged) <- optimize' (d + 1) (openN [x', f'] t)
  return (Fix p f fty x xty (closeN [x', f'] t'), tChanged)
recursiveOptimize d (IfZ p c t e) = do
  (c', cChanged) <- optimize' (d + 1) c
  (t', tChanged) <- optimize' (d + 1) t
  (e', eChanged) <- optimize' (d + 1) e
  return (IfZ p c' t' e', cChanged || tChanged || eChanged)
recursiveOptimize d (Let p n ty e t) = do
  let n' = n ++ show d
  (e', eChanged) <- optimize' (d + 1) e
  (t', tChanged) <- optimize' (d + 1) (open n' t)
  return (Let p n ty e' (close n' t'), eChanged || tChanged)
recursiveOptimize d (BinaryOp p op t u) = do
  (t', tChanged) <- optimize' (d + 1) t
  (u', uChanged) <- optimize' (d + 1) u
  return (BinaryOp p op t' u', tChanged || uChanged)
recursiveOptimize _ t@(Const _ _) = return (t, False)
recursiveOptimize _ t@(V _ _) = return (t, False)

constantFolding :: MonadFD4 m => Optimizer m
constantFolding (BinaryOp _ Add (Const _ (CNat 0)) u) = return (u, True)
constantFolding (BinaryOp _ _ t (Const _ (CNat 0))) = return (t, True)
constantFolding (BinaryOp p op (Const _ (CNat n)) (Const _ (CNat m))) = return (Const p (CNat (semOp op n m)), True)
constantFolding (IfZ _ (Const _ (CNat 0)) t _) = return (t, True)
constantFolding (IfZ _ (Const _ (CNat _)) _ e) = return (e, True)
constantFolding term = return (term, False)

constantPropagation :: MonadFD4 m => Optimizer m
constantPropagation (Let _ _ _ c@(Const _ _) t) = return (subst c t, True)
constantPropagation term = return (term, False)

inlineExpansion :: MonadFD4 m => Optimizer m
inlineExpansion (Let _ _ _ e t) = return (subst e t, True)
inlineExpansion (App _ (Lam _ _ _ b) t) = return (subst t b, True)
inlineExpansion term = return (term, False)
