{-# LANGUAGE PatternSynonyms #-}

-- |
-- Module      : Byecompile
-- Description : Compila a bytecode. Ejecuta bytecode.
-- Copyright   : (c) Mauro Jaskelioff, Guido Martínez, 2020.
-- License     : GPL-3
-- Maintainer  : mauro@fceia.unr.edu.ar
-- Stability   : experimental
--
-- Este módulo permite compilar módulos a la BVM. También provee una implementación de la BVM
-- para ejecutar bytecode.
module Bytecompile (Bytecode, runBC, bcWrite, bcRead, bytecompileModule) where

import Data.Binary
  ( Binary (get, put),
    Word32,
    decode,
    encode,
  )
import Data.Binary.Get (getWord32le, isEmpty)
import Data.Binary.Put (putWord32le)
import qualified Data.ByteString.Lazy as BS
import Data.Char (ord)
import GHC.Char (chr)
import Lang
import MonadFD4 (MonadFD4, printFD4, putCharFD4)
import Subst (close)

type Opcode = Int

type Bytecode = [Int]

newtype Bytecode32 = BC {un32 :: [Word32]}

{- Esta instancia explica como codificar y decodificar Bytecode de 32 bits -}
instance Binary Bytecode32 where
  put (BC bs) = mapM_ putWord32le bs
  get = go
    where
      go =
        do
          empty <- isEmpty
          if empty
            then return $ BC []
            else do
              x <- getWord32le
              BC xs <- go
              return $ BC (x : xs)

{- Estos sinónimos de patrón nos permiten escribir y hacer
pattern-matching sobre el nombre de la operación en lugar del código
entero, por ejemplo:

   f (CALL : cs) = ...

 Notar que si hubieramos escrito algo como
   call = 5
 no podríamos hacer pattern-matching con `call`.

 En lo posible, usar estos códigos exactos para poder ejectutar un
 mismo bytecode compilado en distintas implementaciones de la máquina.
-}
pattern NULL = 0

pattern RETURN = 1

pattern CONST = 2

pattern ACCESS = 3

pattern FUNCTION = 4

pattern CALL = 5

pattern ADD = 6

pattern SUB = 7

pattern IFZ = 8

pattern FIX = 9

pattern STOP = 10

pattern SHIFT = 11

pattern DROP = 12

pattern PRINT = 13

pattern PRINTN = 14

pattern JUMP = 15

pattern TAILCALL = 16

data Val = I Int | Fun Env Bytecode | RA Env Bytecode

type Env = [Val]

type Stack = [Val]

bc :: MonadFD4 m => Term -> m Bytecode
bc (V _ (Bound i)) = return [ACCESS, i]
bc (V _ _) = undefined
bc (Const _ (CNat n)) = return [CONST, n]
bc (Lam _ _ _ t) = do
  t' <- bcT t
  return $ FUNCTION : length t' : t'
bc (App _ t u) = do
  -- NOTE: (\t u -> t ++ u ++ [CALL]) <$> bc t <*> bc u
  t' <- bc t
  u' <- bc u
  return $ t' ++ u' ++ [CALL]
bc (Print _ str t) = do
  t' <- bc t
  return $ [PRINT] ++ map ord str ++ [NULL] ++ t' ++ [PRINTN]
bc (BinaryOp _ op t u) = do
  t' <- bc t
  u' <- bc u
  let cop = case op of Add -> ADD; Sub -> SUB
  return $ t' ++ u' ++ [cop]
bc (Fix _ _ _ _ _ t) = do
  t' <- bcT t
  return $ FUNCTION : length t' : t' ++ [FIX]
bc (IfZ _ c t e) = do
  c' <- bc c
  t' <- bc t
  e' <- bc e
  return $ c' ++ [IFZ, length t' + 2] ++ t' ++ [JUMP, length e'] ++ e'
bc (Let _ _ _ e t) = do
  e' <- bc e
  t' <- bc t
  return $ e' ++ [SHIFT] ++ t' ++ [DROP]

bcT :: MonadFD4 m => Term -> m Bytecode
bcT (App _ t u) = do
  t' <- bc t
  u' <- bc u
  return $ t' ++ u' ++ [TAILCALL]
bcT (IfZ _ c t e) = do
  c' <- bc c
  t' <- bcT t
  e' <- bcT e
  return $ c' ++ [IFZ, length t' + 2] ++ t' ++ [JUMP, length e'] ++ e'
bcT (Let _ _ _ e t) = do
  e' <- bc e
  t' <- bcT t
  return $ e' ++ [SHIFT] ++ t'
bcT t = do
  t' <- bc t
  return $ t' ++ [RETURN]

type Module = [DeclTerm]

bytecompileModule :: MonadFD4 m => Module -> m Bytecode
bytecompileModule decls = do
  t <- transform decls
  bytec <- bc t
  return $ bytec ++ [PRINTN, STOP]
  where
    transform :: MonadFD4 m => Module -> m Term
    transform [] = error "No main function"
    transform [Decl _ _ _ t] = return t
    transform (Decl p n ty t : ds) = do
      ds' <- transform ds
      return $ Let p n ty t (close n (glob2free ds'))

-- | Toma un bytecode, lo codifica y lo escribe un archivo
bcWrite :: Bytecode -> FilePath -> IO ()
bcWrite bs filename = BS.writeFile filename (encode $ BC $ fromIntegral <$> bs)

---------------------------

-- * Ejecución de bytecode

---------------------------

-- | Lee de un archivo y lo decodifica a bytecode
bcRead :: FilePath -> IO Bytecode
bcRead filename = map fromIntegral <$> un32 <$> decode <$> BS.readFile filename

runBC :: MonadFD4 m => Bytecode -> m ()
runBC c = runBC' c [] []

runBC' :: MonadFD4 m => Bytecode -> Env -> Stack -> m ()
runBC' [] _ _ = return ()
runBC' (CONST : n : cs) env stack = runBC' cs env (I n : stack)
runBC' (ADD : cs) env (I n : I m : stack) = runBC' cs env (I (m + n) : stack)
runBC' (SUB : cs) env (I n : I m : stack) = runBC' cs env (I (max (m - n) 0) : stack)
runBC' (ACCESS : i : cs) env stack = runBC' cs env (env !! i : stack)
runBC' (CALL : cs) env (v : Fun ef cf : stack) = runBC' cf (v : ef) (RA env cs : stack)
runBC' (FUNCTION : skip : cs) env stack = runBC' (drop skip cs) env (Fun env (take skip cs) : stack)
runBC' (RETURN : cs) _ (v : RA ef cf : stack) = runBC' cf ef (v : stack)
runBC' (SHIFT : cs) env (v : stack) = runBC' cs (v : env) stack
runBC' (DROP : cs) (_ : env) stack = runBC' cs env stack
runBC' (PRINTN : cs) env stack@(I n : _) = printFD4 (show n) >> runBC' cs env stack
runBC' (PRINT : cs) env stack = do
  cs' <- go cs
  runBC' cs' env stack
  where
    go [] = undefined
    go (NULL : k) = return k
    go (char : k) = putCharFD4 (chr char) >> go k
runBC' (FIX : cs) env (Fun ef cf : stack) = runBC' cs env (Fun efx cf : stack)
  where
    efx = Fun efx cf : env
runBC' (IFZ : _ : cs) env (I 0 : stack) = runBC' cs env stack
runBC' (IFZ : skipt : cs) env (_ : stack) = runBC' (drop skipt cs) env stack
runBC' (JUMP : skip : cs) env stack = runBC' (drop skip cs) env stack
runBC' (TAILCALL : _) env (v : Fun ef cf : stack) = runBC' cf (v : ef) stack
runBC' [STOP] _ _ = return ()
runBC' _ _ _ = undefined
