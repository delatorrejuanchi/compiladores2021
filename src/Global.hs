-- |
-- Module      : Global
-- Description : Define el estado global del compilador
-- Copyright   : (c) Mauro Jaskelioff, Guido Martínez, 2020.
-- License     : GPL-3
-- Maintainer  : mauro@fceia.unr.edu.ar
-- Stability   : experimental
module Global where

import Lang

data GlEnv = GlEnv
  { -- | True, si estamos en modo interactivo.
    inter :: Bool,
    -- | Último archivo cargado.
    lfile :: String,
    -- | Cantidad de declaraciones desde la última carga
    cantDecl :: Int,
    -- | Entorno con declaraciones globales
    glb :: [DeclTerm],
    -- | Entorno de tipado de declaraciones globales
    tyEnv :: [(Name, Ty)],
    -- | Entorno de sinonimos de tipos
    typeSynonyms :: [(Name, Ty)]
  }

-- | Valor del estado inicial
initialEnv :: GlEnv
initialEnv = GlEnv True "" 0 [] [] []
