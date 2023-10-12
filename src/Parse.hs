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
import           Control.Monad.RWS                      (MonadState (get))
import           Data.Char                              (isNumber, ord)
import           Elab                                   (buildFunTy)
import           Lang
import           Prelude                                hiding (const)
import           Text.Parsec                            hiding (oneOf, parse,
                                                         runP)
import           Text.Parsec.Expr                       (Assoc, Operator)
import qualified Text.Parsec.Expr                       as Ex
import qualified Text.Parsec.Token                      as Tok
import           Text.ParserCombinators.Parsec.Language

oneOf :: [P a] -> P a
oneOf = choice . map try

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
            "Nat"
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
tyatom = (reserved "Nat" >> return SNatTy)
         <|> (STySyn <$> var)
         <|> parens typeP


typeP :: P STy
typeP = oneOf [SFunTy <$> tyatom <*> (reservedOp "->" >> typeP), tyatom]

const :: P Const
const = CNat <$> num

printOp :: P SNTerm
printOp = do
  i <- getPos
  reserved "print"
  str <- option "" stringLiteral
  oneOf [SPrint i str <$> expr, return $ SPrintEta i str]

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
  t <- expr
  return (SLam i bs t)

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
  e <- expr
  return (SIfZ i c t e)

fix :: P SNTerm
fix = do
  i <- getPos
  reserved "fix"
  (f, fty) <- parens binding
  (x, xty) <- parens binding
  reservedOp "->"
  t <- expr
  return (SFix i f fty x xty t)

letexp :: P SNTerm
letexp = oneOf [letvar, letfun, letrec]

letvar :: P SNTerm
letvar = do
  i <- getPos
  reserved "let"
  (v, ty) <- oneOf [parens binding, binding]
  reservedOp "="
  def <- expr
  reserved "in"
  body <- expr
  return (SLet i v ty def body)

letfun :: P SNTerm
letfun = do
  i <- getPos
  reserved "let"
  f <- var
  bs <- many1 $ parens binding
  reservedOp ":"
  rty <- typeP
  reservedOp "="
  def <- expr
  reserved "in"
  body <- expr
  return (SLetFun i f bs rty def body)

letrec :: P SNTerm
letrec = do
  i <- getPos
  reserved "let"
  reserved "rec"
  f <- var
  bs <- many1 $ parens binding
  reservedOp ":"
  rty <- typeP
  reservedOp "="
  def <- expr
  reserved "in"
  body <- expr
  return (SLetRec i f bs rty def body)

-- | Parser de términos
tm :: P SNTerm
tm = oneOf [app, lam, ifz, printOp, fix, letexp]

-- | Parser de declaraciones

-- TODO: do not build function type and term here
declfun :: P (Decl STy SNTerm)
declfun = do
  i <- getPos
  reserved "let"
  f <- var
  bs <- many1 $ parens binding
  reservedOp ":"
  rty <- typeP
  reservedOp "="
  def <- expr
  let ty = buildFunTy bs rty
  return $ Decl i f ty (SLam i bs def)

-- TODO: do not build function type and term here
declfunrec :: P (Decl STy SNTerm)
declfunrec = do
  i <- getPos
  reserved "let"
  reserved "rec"
  f <- var
  bs <- many1 $ parens binding
  reservedOp ":"
  rty <- typeP
  reservedOp "="
  def <- expr
  let ty = buildFunTy bs rty
  return $ Decl i f ty $ uncurry (SFix i f ty) (head bs) (SLam i (tail bs) def)

declvar :: P (Decl STy SNTerm)
declvar = do
  i <- getPos
  reserved "let"
  (v, ty) <- binding
  reservedOp "="
  t <- expr
  return (Decl i v ty t)

decltype :: P (Decl STy SNTerm)
decltype = do
  i <- getPos
  reserved "type"
  ty <- var
  reservedOp "="
  tydef <- typeP
  return (DeclTy i ty tydef)

decl :: P (Decl STy SNTerm)
decl = oneOf [decltype, declfun, declfunrec, declvar]

-- | Parser de programas (listas de declaraciones)
program :: P [Decl STy SNTerm]
program = many decl

-- | Parsea una declaración a un término
-- Útil para las sesiones interactivas
declOrTm :: P (Either (Decl STy SNTerm) SNTerm)
declOrTm = try (Left <$> decl) <|> (Right <$> expr)

-- Corre un parser, chequeando que se pueda consumir toda la entrada
runP :: P a -> String -> String -> Either ParseError a
runP p s filename = runParser (whiteSpace *> p <* eof) () filename s

--para debugging en uso interactivo (ghci)
parse :: String -> SNTerm
parse s = case runP expr s "" of
  Right t -> t
  Left e  -> error ("no parse: " ++ show s)
