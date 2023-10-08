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

module Elab ( elab, elab_decl ) where

import           Lang
import           Subst

buildFunTy :: [(Name, STy)] -> STy -> STy
buildFunTy [] rty             = rty
buildFunTy ((_, ty) : bs) rty = SFunTy ty (buildFunTy bs rty)

desugar :: SNTerm -> NTerm
desugar (SV i v) = V i v
desugar (SConst i c) = Const i c
desugar (SLam i bs t) = foldr (\(x, xty) acc -> Lam i x (desugarType xty) acc) (desugar t) bs
desugar (SApp i t u) = App i (desugar t) (desugar u)
desugar (SPrint i str t) = Print i str (desugar t)
desugar (SPrintEta i str) = desugar $ SLam i [("x", SNatTy)] (SPrint i str (SV i "x"))
desugar (SBinaryOp i o t u) = BinaryOp i o (desugar t) (desugar u)
desugar (SFix i f fty x xty t) = Fix i f (desugarType fty) x (desugarType xty) (desugar t)
desugar (SIfZ i c t e) = IfZ i (desugar c) (desugar t) (desugar e)
desugar (SLet i v vty def body) = Let i v (desugarType vty) (desugar def) (desugar body)
desugar (SLetFun i f bs fty def body) = desugar $ SLet i f (buildFunTy bs fty) (SLam i bs def) body
desugar (SLetRec i f [] fty def body) = error "desugar: no binders in letrec"
desugar (SLetRec i f [(x, xty)] fty def body) = desugar $ SLet i f (SFunTy xty fty) (SFix i f (SFunTy xty fty) x xty def) body
desugar (SLetRec i f ((x, xty) : bs) fty def body) = desugar $ SLetRec i f [(x, xty)] (buildFunTy bs fty) (SLam i bs def) body

desugarType :: STy -> Ty
desugarType SNatTy             = NatTy
desugarType (SFunTy sty1 sty2) = FunTy (desugarType sty1) (desugarType sty2)
desugarType (STySyn sty)       = undefined -- TODO: implement

-- | 'elab' transforma variables ligadas en índices de de Bruijn
-- en un término dado.
elab :: SNTerm -> Term
elab = elab' [] . desugar

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

elab_decl :: Decl SNTerm -> Decl Term
elab_decl = fmap elab
