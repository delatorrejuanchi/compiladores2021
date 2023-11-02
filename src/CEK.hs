-- |
-- Module      : CEK
-- Description : Define una maquina abstracta CEK.
-- Copyright   : (c) Juan Cruz de la Torre, Bautista Marelli, 2023.
-- License     : GPL-3
-- Maintainer  : none
-- Stability   : experimental
module CEK where

import Common
import Eval
import Lang
import MonadFD4
import PPrint
import Subst (substN)

data Val = ValNum Int | ValClos Clos deriving (Show)

type Env = [Val]

data Clos
  = ClosFun Env Term Term
  | ClosFix Env Term Term
  deriving (Show)

data Frame
  = KArg Env Term
  | KClos Clos
  | KIFz Env Term Term
  | KOpL Env BinaryOp Term
  | KOpR BinaryOp Val
  | KPrint String
  | KLet Env Term
  deriving (Show)

type Kont = [Frame]

search :: MonadFD4 m => Term -> Env -> Kont -> m Val
search (Print _ s t) p k = search t p (KPrint s : k)
search (BinaryOp _ op t u) p k = search t p (KOpL p op u : k)
search (IfZ _ c t e) p k = search c p (KIFz p t e : k)
search (App _ t u) p k = search t p (KArg p u : k)
search (V _ (Bound i)) p k = destroy (p !! i) k
search (V _ (Free _)) p k = undefined
search (V _ (Global nm)) p k = do
  t <- maybeGetDecl nm
  case t of
    Just t' -> search t' p k
    Nothing -> failFD4 $ "Error de ejecución: Variable no declarada: " ++ ppName nm
search (Const _ (CNat n)) p k = destroy (ValNum n) k
search f@(Lam _ _ _ t) p k = destroy (ValClos (ClosFun p t f)) k
search (Let _ _ _ t u) p k = search t p (KLet p u : k)
search f@(Fix _ _ _ _ _ t) p k = destroy (ValClos (ClosFix p t f)) k

destroy :: MonadFD4 m => Val -> Kont -> m Val
destroy v@(ValNum n) (KPrint s : k) = printFD4 (s ++ show n) >> destroy v k
destroy v@(ValNum n) (KOpL p op u : k) = search u p (KOpR op v : k)
destroy (ValNum n) (KOpR op (ValNum m) : k) = destroy (ValNum (semOp op m n)) k
destroy (ValNum 0) (KIFz p t e : k) = search t p k
destroy (ValNum _) (KIFz p t e : k) = search e p k
destroy (ValClos clos) (KArg p u : k) = search u p (KClos clos : k)
destroy v (KClos (ClosFun p t _) : k) = search t (v : p) k
destroy v (KClos f@(ClosFix p t _) : k) = search t (v : ValClos f : p) k
destroy v (KLet p t : k) = search t (v : p) k
destroy v [] = return v
destroy _ _ = undefined

evalCEK :: MonadFD4 m => Term -> m Term
evalCEK t = do
  v <- search t [] []
  return $ toTerm v
  where
    toTerm :: Val -> Term
    toTerm (ValNum n) = Const NoPos (CNat n)
    toTerm (ValClos (ClosFun p _ f)) = substN' (map toTerm p) f
    toTerm (ValClos (ClosFix p _ f)) = substN' (map toTerm p) f

    substN' :: [Term] -> Term -> Term
    substN' = substN . reverse