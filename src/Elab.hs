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

module Elab ( elab, elab_decl, desugarType, buildFunTy ) where

import           Lang
import           Subst
import MonadFD4

buildFunTy :: [(Name, STy)] -> STy -> STy
buildFunTy [] rty             = rty
buildFunTy ((_, ty) : bs) rty = SFunTy ty (buildFunTy bs rty)

desugarFunTys :: MonadFD4 m => [(Name, STy)] -> m [(Name, Ty)]
desugarFunTys = traverse (\(x, xty) -> do { xty' <- desugarType xty; return (x, xty') })

desugar :: MonadFD4 m => SNTerm -> m NTerm
desugar (SV i v) = return $ V i v
desugar (SConst i c) = return $ Const i c
desugar (SLam i bs t) = do
  t' <- desugar t
  bs' <- desugarFunTys bs -- TODO: do not iterate twice
  return $ foldr (\(x, xty) acc -> Lam i x xty acc) t' bs'
desugar (SApp i t u) = do
  t' <- desugar t
  u' <- desugar u
  return $ App i t' u'
desugar (SPrint i str t) = do
  t' <- desugar t
  return $ Print i str t'
desugar (SPrintEta i str) = desugar $ SLam i [("x", SNatTy)] (SPrint i str (SV i "x"))
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
desugar (SLet i v vty def body) = do
  vty' <- desugarType vty
  def' <- desugar def
  body' <- desugar body
  return $ Let i v vty' def' body'
desugar (SLetFun i f bs fty def body) = desugar $ SLet i f (buildFunTy bs fty) (SLam i bs def) body
desugar (SLetRec i f [] fty def body) = error "desugar: no binders in letrec"
desugar (SLetRec i f [(x, xty)] fty def body) = desugar $ SLet i f (SFunTy xty fty) (SFix i f (SFunTy xty fty) x xty def) body
desugar (SLetRec i f ((x, xty) : bs) fty def body) = desugar $ SLetRec i f [(x, xty)] (buildFunTy bs fty) (SLam i bs def) body

desugarType :: MonadFD4 m => STy -> m Ty
desugarType SNatTy = return NatTy
desugarType (SFunTy ty1 ty2) = do
  ty1' <- desugarType ty1
  ty2' <- desugarType ty2
  return $ FunTy ty1' ty2'
desugarType (STySyn name) = do
  ty <- lookupTy name
  case ty of Nothing -> failFD4 $ "unknown type synonym for " ++ show ty
             Just ty' -> return ty'

-- | 'elab' transforma variables ligadas en índices de de Bruijn
-- en un término dado.
elab :: MonadFD4 m => SNTerm -> m Term
elab t = elab' [] <$> desugar t

elab' :: [Name] -> NTerm -> Term
elab' env (V p v) =
  -- Tenemos que hver si la variable es Global o es un nombre local
  -- En env llevamos la lista de nombres locales.
  if v `elem` env
    then  V p (Free v)
    else V p (Global v)

elab' _ (Const p c) = Const p c
elab' env (Lam p v ty t) = Lam p v ty (close v (elab' (v:env) t))
elab' env (Fix p f fty x xty t) = Fix p f fty x xty (closeN [f, x] (elab' (x:f:env) t))
elab' env (IfZ p c t e)         = IfZ p (elab' env c) (elab' env t) (elab' env e)
-- Operador Print
elab' env (Print i str t) = Print i str (elab' env t)
-- Operadores binarios
elab' env (BinaryOp i o t u) = BinaryOp i o (elab' env t) (elab' env u)
-- Aplicaciones generales
elab' env (App p h a) = App p (elab' env h) (elab' env a)
elab' env (Let p v vty def body) = Let p v vty (elab' env def) (close v (elab' (v:env) body))

elab_decl :: MonadFD4 m => Decl STy SNTerm -> m (Decl Ty Term)
elab_decl (Decl p n ty t) = do
  ty' <- desugarType ty
  t' <- elab t
  return $ Decl p n ty' t'
elab_decl (DeclTy p n ty) = DeclTy p n <$> desugarType ty
