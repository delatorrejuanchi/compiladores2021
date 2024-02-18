module ClosureConvert where

import Control.Monad.State
import Control.Monad.Writer
import IR
import Lang
import Subst (open, openN)

newName :: Name -> StateT Int (Writer [IrDecl]) Name
newName n = do
  i <- get
  put (i + 1)
  return $ n ++ show i

closeIr :: [Name] -> Ir -> Name -> Ir
closeIr nns t clos = foldr (\(x, i) ir -> IrLet x (IrAccess (IrVar clos) i) ir) t xs
  where
    xs = zip nns [1 ..]

closureConvert :: Term -> StateT Int (Writer [IrDecl]) Ir
closureConvert (V _ (Free n)) = return $ IrVar n
closureConvert (V _ (Global n)) = return $ IrGlobal n
closureConvert (V _ (Bound _)) = undefined
closureConvert (Const _ c) = return $ IrConst c
closureConvert (App _ t u) = do
  nn <- newName "clos_"
  t' <- closureConvert t
  u' <- closureConvert u
  return $ IrLet nn t' (IrCall (IrAccess (IrVar nn) 0) [IrVar nn, u'])
closureConvert (Print _ s t) = IrPrint s <$> closureConvert t
closureConvert (BinaryOp _ op t u) = do
  t' <- closureConvert t
  u' <- closureConvert u
  return $ IrBinaryOp op t' u'
closureConvert (IfZ _ c t e) = do
  c' <- closureConvert c
  t' <- closureConvert t
  e' <- closureConvert e
  return $ IrIfZ c' t' e'
closureConvert (Let _ n _ e t) = do
  e' <- closureConvert e
  nn <- newName n
  t' <- closureConvert (open nn t)
  return $ IrLet nn e' t'
closureConvert (Lam _ n ty t) = do
  nn <- newName n
  t' <- closureConvert (open nn t)
  fname <- newName "function"
  let fv = freeVars t
      clname = fname ++ "_args"
      ff = closeIr fv t' clname
      codef = IrFun fname [clname, nn] ff
  tell [codef]
  return $ MkClosure fname (map IrVar fv)
closureConvert (Fix _ f _ x _ t) = do
  xnn <- newName x
  fnn <- newName $ "rec_" ++ f
  let clname = fnn ++ "_args"
  t' <- closureConvert (openN [clname, xnn] t)
  let fv = freeVars t
      ff = closeIr fv t' clname
      codef = IrFun fnn [clname, xnn] ff
  tell [codef]
  return $ MkClosure fnn (map IrVar fv)

runCC :: [DeclTerm] -> [IrDecl]
runCC decls = snd $ runWriter $ runStateT (go decls) 0
  where
    go [] = return []
    go (Decl _ n _ t : ds) = do
      t' <- closureConvert t
      let irDecl = IrVal n t'
      tell [irDecl]
      go ds
