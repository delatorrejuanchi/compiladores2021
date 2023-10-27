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

import Common (Pos)
import Data.List.NonEmpty (head, tail)
import Lang
import MonadFD4 (MonadFD4)
import Subst

buildFunTy :: Binders -> STy -> STy
buildFunTy bs rty = foldr (\(_, xty) acc -> SFunTy xty acc) rty bs

buildFun :: Foldable f => Pos -> f (Name, STy) -> Tm Pos Name STy -> Tm Pos Name STy
buildFun i bs t = foldr (\(x, xty) acc -> Lam i x xty acc) t bs

buildRecFun :: Pos -> Name -> Binders -> STy -> Tm Pos Name STy -> Tm Pos Name STy
buildRecFun i f bs rty def = Fix i f fty x xty (buildFun i (Data.List.NonEmpty.tail bs) def) where (x, xty) = Data.List.NonEmpty.head bs; fty = buildFunTy bs rty

transform :: SNTerm -> Tm Pos Name STy
transform (SV i x) = V i x
transform (SConst i c) = Const i c
transform (SApp i t u) = App i (transform t) (transform u)
transform (SPrint i str t) = Print i str (transform t)
transform (SBinaryOp i o t u) = BinaryOp i o (transform t) (transform u)
transform (SFix i f fty x xty t) = Fix i f fty x xty (transform t)
transform (SIfZ i c t e) = IfZ i (transform c) (transform t) (transform e)
transform (SLet i x xty def body) = Let i x xty (transform def) (transform body)
transform (SLam i bs t) = buildFun i bs (transform t)
transform (SPrintEta i str) = Lam i "x" SNatTy (Print i str (V i "x"))
transform (SLetFun i f bs rty def body) = Let i f (buildFunTy bs rty) (buildFun i bs $ transform def) (transform body)
transform (SLetRec i f bs rty def body) = Let i f (buildFunTy bs rty) (buildRecFun i f bs rty $ transform def) (transform body)

desugarType :: MonadFD4 m => STy -> m Ty
desugarType SNatTy = return NatTy
desugarType (SFunTy ty1 ty2) = do
  ty1' <- desugarType ty1
  ty2' <- desugarType ty2
  return $ FunTy ty1' ty2'

elab :: MonadFD4 m => SNTerm -> m Term
elab t = elab' [] <$> traverse desugarType (transform t)

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

transformDecl :: SDecl -> Decl Name STy
transformDecl (SDeclVar i v ty t) = Decl i v ty (transform t)
transformDecl (SDeclFun i f bs rty def) = Decl i f (buildFunTy bs rty) (buildFun i bs (transform def))
transformDecl (SDeclRec i f bs rty def) = Decl i f (buildFunTy bs rty) (buildRecFun i f bs rty (transform def))

elabDecl :: MonadFD4 m => SDecl -> m DeclTerm
elabDecl d = elabDecl' <$> traverse desugarType (transformDecl d)

elabDecl' :: DeclNTerm -> DeclTerm
elabDecl' (Decl p v ty t) = Decl p v ty (elab' [] t)
