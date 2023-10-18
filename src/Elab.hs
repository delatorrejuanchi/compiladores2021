-- |
-- Module      : Elab
-- Description : Elabora un término fully named a uno locally closed.
-- Copyright   : (c) Mauro Jaskelioff, Guido Martínez, 2020.
-- License     : GPL-3
-- Maintainer  : mauro@fceia.unr.edu.ar
-- Stability   : experimental
--
-- Este módulo permite elaborar términos y declaraciones para convertirlas desde
-- fully named (@NTerm) a locally closed (@Term@)
module Elab (elab, elabDecl) where

import Data.List.NonEmpty (NonEmpty ((:|)))
import Lang
import MonadFD4 (MonadFD4)
import Subst

buildFunTy :: Binders -> STy -> STy
buildFunTy ((_, xty) :| []) rty = SFunTy xty rty
buildFunTy ((_, xty) :| b : bb) rty = SFunTy xty (buildFunTy (b :| bb) rty)

desugar :: MonadFD4 m => SNTerm -> m NTerm
desugar (SV i v) = return $ V i v
desugar (SConst i c) = return $ Const i c
desugar (SLam i bs t) = do
  t' <- desugar t
  bs' <- mapM (\(x, xty) -> (,) x <$> desugarType xty) bs
  return $ foldr (\(x, xty) acc -> Lam i x xty acc) t' bs'
desugar (SApp i t u) = do
  t' <- desugar t
  u' <- desugar u
  return $ App i t' u'
desugar (SPrint i str t) = do
  t' <- desugar t
  return $ Print i str t'
desugar (SBinaryOp i o t u) = do
  t' <- desugar t
  u' <- desugar u
  return $ BinaryOp i o t' u'
desugar (SFix i f fty x xty t) = do
  fty' <- desugarType fty
  xty' <- desugarType xty
  t' <- desugar t
  return $ Fix i f fty' x xty' t'
desugar (SIfZ i c t e) = do
  c' <- desugar c
  t' <- desugar t
  e' <- desugar e
  return $ IfZ i c' t' e'
desugar (SLet i x xty def body) = do
  xty' <- desugarType xty
  def' <- desugar def
  body' <- desugar body
  return $ Let i x xty' def' body'
desugar (SPrintEta i str) = desugar $ SLam i (("x", SNatTy) :| []) (SPrint i str (SV i "x"))
desugar (SLetFun i f bs rty def body) = desugar $ SLet i f (buildFunTy bs rty) (SLam i bs def) body
desugar (SLetRec i f ((x, xty) :| []) rty def body) = let fty = SFunTy xty rty in desugar $ SLet i f fty (SFix i f fty x xty def) body
desugar (SLetRec i f ((x, xty) :| b : bb) rty def body) = desugar $ SLetRec i f ((x, xty) :| []) (buildFunTy (b :| bb) rty) (SLam i (b :| bb) def) body

desugarType :: MonadFD4 m => STy -> m Ty
desugarType SNatTy = return NatTy
desugarType (SFunTy ty1 ty2) = do
  ty1' <- desugarType ty1
  ty2' <- desugarType ty2
  return $ FunTy ty1' ty2'

elab :: MonadFD4 m => SNTerm -> m Term
elab t = elab' [] <$> desugar t

elab' :: [Name] -> NTerm -> Term
elab' _ (Const p c) = Const p c
elab' env (V p v) = if v `elem` env then V p (Free v) else V p (Global v)
elab' env (Lam p v ty t) = Lam p v ty (close v (elab' (v : env) t))
elab' env (Fix p f fty x xty t) = Fix p f fty x xty (closeN [f, x] (elab' (x : f : env) t))
elab' env (IfZ p c t e) = IfZ p (elab' env c) (elab' env t) (elab' env e)
elab' env (Print i str t) = Print i str (elab' env t)
elab' env (BinaryOp i o t u) = BinaryOp i o (elab' env t) (elab' env u)
elab' env (App p h a) = App p (elab' env h) (elab' env a)
elab' env (Let p v vty def body) = Let p v vty (elab' env def) (close v (elab' (v : env) body))

desugarDecl :: MonadFD4 m => SDecl -> m (Decl NTerm)
desugarDecl (SDeclVar i x xty def) = do
  xty' <- desugarType xty
  def' <- desugar def
  return $ Decl i x xty' def'
desugarDecl (SDeclFun i f bs rty def) = do
  fty <- desugarType (buildFunTy bs rty)
  def' <- desugar (SLam i bs def)
  return $ Decl i f fty def'
desugarDecl (SDeclRec i f ((x, xty) :| []) rty def) = do
  let fty = SFunTy xty rty
  fty' <- desugarType fty
  def' <- desugar (SFix i f fty x xty def)
  return $ Decl i f fty' def'
desugarDecl (SDeclRec i f bs@((x, xty) :| b : bb) rty def) = desugarDecl $ SDeclRec i f ((x, xty) :| []) (buildFunTy (b :| bb) rty) (SLam i (b :| bb) def)

elabDecl :: MonadFD4 m => SDecl -> m (Decl Term)
elabDecl d = fmap (elab' []) <$> desugarDecl d
