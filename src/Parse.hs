-- |
-- Module      : Parse
-- Description : Define un parser de términos FD40 a términos fully named.
-- Copyright   : (c) Mauro Jaskelioff, Guido Martínez, 2020.
-- License     : GPL-3
-- Maintainer  : mauro@fceia.unr.edu.ar
-- Stability   : experimental
module Parse (tm, Parse.parse, decl, runP, P, program, declOrTm) where

import           Common
--( GenLanguageDef(..), emptyDef )

import           Control.Monad.Identity                 (Identity)
import           Data.Char                              (isNumber, ord)
import           Lang
import           Prelude                                hiding (const)
import           Text.Parsec                            hiding (oneOf, parse,
                                                         runP)
import           Text.Parsec.Expr                       (Assoc, Operator)
import qualified Text.Parsec.Expr                       as Ex
import qualified Text.Parsec.Token                      as Tok
import           Text.ParserCombinators.Parsec.Language

type P = Parsec String ()

-----------------------
-- Lexer
-----------------------

-- | Analizador de Tokens
lexer :: Tok.TokenParser u
lexer =
  Tok.makeTokenParser $
    emptyDef
      { commentLine = "#",
        reservedNames =
          [ "let",
            "rec",
            "fun",
            "fix",
            "then",
            "else",
            "in",
            "ifz",
            "print",
            "Nat",
            "type"
          ],
        reservedOpNames = ["->", ":", "=", "+", "-"]
      }

whiteSpace :: P ()
whiteSpace = Tok.whiteSpace lexer

natural :: P Integer
natural = Tok.natural lexer

stringLiteral :: P String
stringLiteral = Tok.stringLiteral lexer

parens :: P a -> P a
parens = Tok.parens lexer

identifier :: P String
identifier = Tok.identifier lexer

reserved :: String -> P ()
reserved = Tok.reserved lexer

reservedOp :: String -> P ()
reservedOp = Tok.reservedOp lexer

oneOf :: [P a] -> P a
oneOf = choice . map try

-----------------------
-- Parsers
-----------------------

num :: P Int
num = fromInteger <$> natural

var :: P Name
var = identifier

getPos :: P Pos
getPos = do
  pos <- getPosition
  return $ Pos (sourceLine pos) (sourceColumn pos)

tyatom :: P STy
tyatom =
  (reserved "Nat" >> return SNatTy)
    <|> parens typeP

typeP :: P STy
typeP = oneOf [SFunTy <$> tyatom <*> (reservedOp "->" >> typeP), tyatom]

const :: P Const
const = CNat <$> num

-- todo: eta-expansion
printOp :: P SNTerm
printOp = do
  i <- getPos
  reserved "print"
  str <- option "" stringLiteral
  SPrint i str <$> atom

binary :: String -> BinaryOp -> Assoc -> Operator String () Identity SNTerm
binary s f = Ex.Infix (reservedOp s >> return (SBinaryOp NoPos f))

table :: [[Operator String () Identity SNTerm]]
table =
  [ [ binary "+" Add Ex.AssocLeft,
      binary "-" Sub Ex.AssocLeft
    ]
  ]

expr :: P SNTerm
expr = Ex.buildExpressionParser table tm

atom :: P SNTerm
atom = oneOf [SConst <$> getPos <*> const, SV <$> getPos <*> var, parens expr, printOp]

-- parsea un par (variable : tipo)
binding :: P (Name, STy)
binding = do
  v <- var
  reservedOp ":"
  ty <- typeP
  return (v, ty)

lam :: P SNTerm
lam = do
  i <- getPos
  reserved "fun"
  bs <- many1 $ parens binding
  reservedOp "->"
  SLam i bs <$> expr

-- Nota el parser app también parsea un solo atom.
app :: P SNTerm
app = do
  i <- getPos
  f <- atom
  args <- many atom
  return (foldl (SApp i) f args)

ifz :: P SNTerm
ifz = do
  i <- getPos
  reserved "ifz"
  c <- expr
  reserved "then"
  t <- expr
  reserved "else"
  SIfZ i c t <$> expr

fix :: P SNTerm
fix = do
  i <- getPos
  reserved "fix"
  (f, fty) <- parens binding
  (x, xty) <- parens binding
  reservedOp "->"
  SFix i f fty x xty <$> expr

letexp :: P SNTerm
letexp = oneOf [letvar, letfun, letrecfun]

letvar :: P SNTerm
letvar = do
  i <- getPos
  reserved "let"
  (name, ty) <- parens binding <|> binding
  reserved "="
  body <- expr
  reserved "in"
  SLet i False name ty body <$> expr

-- TODO: we should build the rec function closure here
letfun :: P SNTerm
letfun = do
  i <- getPos
  reserved "let"
  name <- var
  bs <- many1 $ parens binding
  reservedOp "="
  body <- expr
  reserved "in"
  let ty = foldr (SFunTy . snd) (snd $ last bs) bs
  let f = SLam i bs body
  SLet i False name ty f <$> expr

letrecfun :: P SNTerm
letrecfun = do
  i <- getPos
  reserved "let"
  reserved "rec"
  name <- var
  bs <- many1 $ parens binding
  reservedOp "="
  body <- expr
  reserved "in"
  let (bname1, bty1) = head bs
  let ty = foldr (SFunTy . snd) (snd $ last bs) bs
  let f = SFix i name ty bname1 bty1 (SLam i bs body)
  SLet i True name ty f <$> expr

-- | Parser de términos
tm :: P SNTerm
tm = oneOf [app, lam, ifz, printOp, fix, letexp]

-- | Parser de declaraciones
decl :: P SNTermDecl
decl = oneOf [declfun, declrecfun, declvar, decltype]

declvar :: P SNTermDecl
declvar = do
  i <- getPos
  reserved "let"
  (name, ty) <- parens binding <|> binding
  reserved "="
  Decl i name ty <$> expr

declfun :: P SNTermDecl
declfun = do
  i <- getPos
  reserved "let"
  name <- var
  bs <- many1 $ parens binding
  reservedOp "="
  let ty = foldr (SFunTy . snd) (snd $ last bs) bs
  Decl i name ty . SLam i bs <$> expr

declrecfun :: P SNTermDecl
declrecfun = do
  i <- getPos
  reserved "let"
  reserved "rec"
  name <- var
  bs <- many1 $ parens binding
  reservedOp "="
  let (bname1, bty1) = head bs
  let ty = foldr (SFunTy . snd) (snd $ last bs) bs
  Decl i name ty . SFix i name ty bname1 bty1 . SLam i bs <$> expr

-- todo: implement
decltype :: P SNTermDecl
decltype = do
  i <- getPos
  reserved "let"
  reserved "type"
  name <- var
  reserved "="
  -- DeclType i name <$> typeP
  undefined

-- | Parser de programas (listas de declaraciones)
program :: P [SNTermDecl]
program = many decl

-- | Parsea una declaración a un término
-- Útil para las sesiones interactivas
declOrTm :: P (Either SNTermDecl SNTerm)
declOrTm = oneOf [Left <$> decl, Right <$> expr]

-- Corre un parser, chequeando que se pueda consumir toda la entrada
runP :: P a -> String -> String -> Either ParseError a
runP p s filename = runParser (whiteSpace *> p <* eof) () filename s

--para debugging en uso interactivo (ghci)
parse :: String -> SNTerm
parse s = case runP expr s "" of
  Right t -> t
  Left e  -> error ("no parse: " ++ show s)
