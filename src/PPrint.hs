{-|
Module      : PPrint
Description : Pretty printer para FD4.
Copyright   : (c) Mauro Jaskelioff, Guido Martínez, 2020.
License     : GPL-3
Maintainer  : mauro@fceia.unr.edu.ar
Stability   : experimental

-}

module PPrint (
    pp,
    ppTy,
    ppName,
    ppDecl
    ) where

import           Lang
import           Subst                         (open, openN)

import           Data.Text                     (unpack)
import           Prettyprinter
import           Prettyprinter.Render.Terminal (AnsiStyle, Color (..), color,
                                                colorDull, italicized,
                                                renderStrict)
 --( (<+>), nest, parens, sep, pretty, Doc, layoutSmart, defaultLayoutOptions, annotate )
import           Global
import           MonadFD4

freshen :: [Name] -> Name -> Name
freshen ns n = let cands = n : (map (\i -> n ++ show i) [0..])
               in head (filter (\m -> not (elem m ns)) cands)

-- | 'openAll' convierte términos locally nameless
-- a términos fully named abriendo todos las variables de ligadura que va encontrando
-- Debe tener cuidado de no abrir términos con nombres que ya fueron abiertos.
-- Estos nombres se encuentran en la lista ns (primer argumento).
openAll :: [Name] -> Term -> NTerm
openAll ns (V p v) = case v of
      Bound i  -> V p $ "(Bound "++show i++")" -- este caso no debería aparecer
                                                -- si el término es localmente cerrado
      Free x   -> V p x
      Global x -> V p x
openAll ns (Const p c) = Const p c
openAll ns (Lam p x ty t) =
  let x' = freshen ns x
  in Lam p x' ty (openAll (x':ns) (open x' t))
openAll ns (App p t u) = App p (openAll ns t) (openAll ns u)
openAll ns (Fix p f fty x xty t) =
  let
    x' = freshen ns x
    f' = freshen (x':ns) f
  in Fix p f' fty x' xty (openAll (x:f:ns) (openN [f',x'] t))
openAll ns (IfZ p c t e) = IfZ p (openAll ns c) (openAll ns t) (openAll ns e)
openAll ns (Print p str t) = Print p str (openAll ns t)
openAll ns (BinaryOp p op t u) = BinaryOp p op (openAll ns t) (openAll ns u)
openAll ns (Let p v ty m n) =
    let v'= freshen ns v
    in  Let p v' ty (openAll ns m) (openAll (v':ns) (open v' n))

--Colores
constColor :: Doc AnsiStyle -> Doc AnsiStyle
constColor = annotate (color Red)
opColor :: Doc AnsiStyle -> Doc AnsiStyle
opColor = annotate (colorDull Green)
keywordColor :: Doc AnsiStyle -> Doc AnsiStyle
keywordColor = annotate (colorDull Green) -- <> bold)
typeColor :: Doc AnsiStyle -> Doc AnsiStyle
typeColor = annotate (color Blue <> italicized)
typeOpColor :: Doc AnsiStyle -> Doc AnsiStyle
typeOpColor = annotate (colorDull Blue)
defColor :: Doc AnsiStyle -> Doc AnsiStyle
defColor = annotate (colorDull Magenta <> italicized)
nameColor :: Doc AnsiStyle -> Doc AnsiStyle
nameColor = id

-- | Pretty printer de nombres (Doc)
name2doc :: Name -> Doc AnsiStyle
name2doc n = nameColor (pretty n)

-- |  Pretty printer de nombres (String)
ppName :: Name -> String
ppName = id

-- | Pretty printer para tipos (Doc)
ty2doc :: Ty -> Doc AnsiStyle
ty2doc NatTy     = typeColor (pretty "Nat")
ty2doc (FunTy x@(FunTy _ _) y) = sep [parens (ty2doc x), typeOpColor (pretty "->"),ty2doc y]
ty2doc (FunTy x y) = sep [ty2doc x, typeOpColor (pretty "->"),ty2doc y]

-- | Pretty printer para tipos (String)
ppTy :: Ty -> String
ppTy = render . ty2doc

c2doc :: Const -> Doc AnsiStyle
c2doc (CNat n) = constColor (pretty (show n))

binary2doc :: BinaryOp -> Doc AnsiStyle
binary2doc Add = opColor (pretty "+")
binary2doc Sub = opColor (pretty "-")

collectApp :: NTerm -> (NTerm, [NTerm])
collectApp t = go [] t where
  go ts (App _ h tt) = go (tt:ts) h
  go ts h            = (h, ts)

parenIf :: Bool -> Doc a -> Doc a
parenIf True = parens
parenIf _    = id

-- t2doc at t :: Doc
-- at: debe ser un átomo
-- | Pretty printing de términos (Doc)
t2doc :: Bool     -- Debe ser un átomo?
      -> NTerm    -- término a mostrar
      -> Doc AnsiStyle
-- Uncomment to use the Show instance for STerm
{- t2doc at x = text (show x) -}
t2doc at (V _ x) = name2doc x
t2doc at (Const _ c) = c2doc c
t2doc at (Lam _ v ty t) =
  parenIf at $
  sep [sep [ keywordColor (pretty "fun")
           , binding2doc (v,ty)
           , opColor(pretty "->")]
      , nest 2 (t2doc False t)]

t2doc at t@(App _ _ _) =
  let (h, ts) = collectApp t in
  parenIf at $
  t2doc True h <+> sep (map (t2doc True) ts)

t2doc at (Fix _ f fty x xty m) =
  parenIf at $
  sep [ sep [keywordColor (pretty "fix")
                  , binding2doc (f, fty)
                  , binding2doc (x, xty)
                  , opColor (pretty "->") ]
      , nest 2 (t2doc False m)
      ]
t2doc at (IfZ _ c t e) =
  parenIf at $
  sep [keywordColor (pretty "ifz"), nest 2 (t2doc False c)
     , keywordColor (pretty "then"), nest 2 (t2doc False t)
     , keywordColor (pretty "else"), nest 2 (t2doc False e) ]

t2doc at (Print _ str t) =
  parenIf at $
  sep [keywordColor (pretty "print"), pretty (show str), t2doc True t]

t2doc at (Let _ v ty t t') =
  parenIf at $
  sep [
    sep [keywordColor (pretty "let")
       , binding2doc (v,ty)
       , opColor (pretty "=") ]
  , nest 2 (t2doc False t)
  , keywordColor (pretty "in")
  , nest 2 (t2doc False t') ]

t2doc at (BinaryOp _ o a b) =
  parenIf at $
  t2doc True a <+> binary2doc o <+> t2doc True b

binding2doc :: (Name, Ty) -> Doc AnsiStyle
binding2doc (x, ty) =
  parens (sep [name2doc x, pretty ":", ty2doc ty])

-- | Pretty printing de términos (String)
pp :: MonadFD4 m => Term -> m String
-- Uncomment to use the Show instance for Term
{- pp = show -}
pp t = do
       gdecl <- gets glb
       return (render . t2doc False $ openAll (map declName gdecl) t)

render :: Doc AnsiStyle -> String
render = unpack . renderStrict . layoutSmart defaultLayoutOptions

-- | Pretty printing de declaraciones
ppDecl :: MonadFD4 m => Decl Term Ty -> m String
ppDecl (Decl p x ty t) = do
  gdecl <- gets glb
  return (render $ sep [defColor (pretty "let")
                       , name2doc x
                       , defColor (pretty "=")]
                   <+> nest 2 (t2doc False $ resugar (openAll (map declName gdecl) t)))
ppDecl (DeclTy p x ty) = return (render $ sep [defColor (pretty "type"), name2doc x, defColor (pretty "="), ty2doc $ resugarType ty])

resugarType :: Ty -> STy
resugarType NatTy           = SNatTy
resugarType (FunTy ty1 ty2) = SFunTy (resugarType ty1) (resugarType ty2)

-- todo: should we add info to info so that we can resugar more closely to what the code looked like?
resugar :: NTerm  -> SNTerm
resugar (V info var) = SV info var
resugar (Const info c) = SConst info c
resugar (Lam info name ty term) = let (args, term') = buildSLam name (resugarType ty) term in SLam info args term'
resugar (App info term1 term2) = SApp info (resugar term1) (resugar term2)
resugar (Print info string term) = SPrint info string (resugar term)
resugar (BinaryOp info op term1 term2) = SBinaryOp info op (resugar term1) (resugar term2)
resugar (Fix info name1 ty1 name2 ty2 body) = SFix info name1 (resugarType ty1) name2 (resugarType ty2) (resugar body)
resugar (IfZ info cond true false) = SIfZ info (resugar cond) (resugar true) (resugar false)
resugar (Let info name ty term1 term2) = SLet info name (resugarType ty) (resugar term1) (resugar term2)

buildSLam :: Name -> STy -> NTerm -> ([(Name, STy)], SNTerm)
buildSLam name ty term = buildSLam' [(name, ty)] term
    where buildSLam' args (Lam info name ty2 term) = buildSLam' ((name, resugarType ty2):args) term
          buildSLam' args term = (reverse args, resugar term)
