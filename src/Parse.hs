{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use <$>" #-}
-- |
-- Module      : Parse
-- Description : Define un parser de términos FD40 a términos fully named.
-- Copyright   : (c) Mauro Jaskelioff, Guido Martínez, 2020.
-- License     : GPL-3
-- Maintainer  : mauro@fceia.unr.edu.ar
-- Stability   : experimental
module Parse (tm, Parse.parse, decl, runP, P, program, declOrTm) where

import           Common
import           Control.Monad.Identity                 (Identity)
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
-- Helpers
-----------------------

oneOf :: [P a] -> P a
oneOf = choice . map try

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

identifier :: P String
identifier = Tok.identifier lexer

parens :: P a -> P a
parens = Tok.parens lexer

reserved :: String -> P ()
reserved = Tok.reserved lexer

reservedOp :: String -> P ()
reservedOp = Tok.reservedOp lexer

-----------------------
-- Parsers
-----------------------

typeP :: P STy
typeP = oneOf [typeFun, typeAtom]

typeFun :: P STy
typeFun = SFunTy <$> typeAtom <*> (reservedOp "->" >> typeP)

typeAtom :: P STy
typeAtom = oneOf [typeNat, parens typeP]

typeNat :: P STy
typeNat = reserved "Nat" >> return SNatTy

num :: P Int
num = fromInteger <$> natural

var :: P Name
var = identifier

const :: P Const
const = CNat <$> num

getPos :: P Pos
getPos = do
  pos <- getPosition
  return $ Pos (sourceLine pos) (sourceColumn pos)

binding :: P (Name, STy)
binding = do
  v <- var
  reservedOp ":"
  ty <- typeP
  return (v, ty)

expr :: P SNTerm
expr = Ex.buildExpressionParser table tm

table :: [[Operator String () Identity SNTerm]]
table = [[binary "+" Add Ex.AssocLeft, binary "-" Sub Ex.AssocLeft]]

binary :: String -> BinaryOp -> Assoc -> Operator String () Identity SNTerm
binary s f = Ex.Infix (reservedOp s >> return (SBinaryOp NoPos f))

tm :: P SNTerm
tm = oneOf [printOp, app, lam, ifz, fix, letexp]

printOp :: P SNTerm
printOp = do
  i <- getPos
  reserved "print"
  str <- option "" stringLiteral
  oneOf [SPrint i str <$> expr, return $ SPrintEta i str]

-- NOTE: app parsea un atom o la aplicación de varios atom.
app :: P SNTerm
app = do
  i <- getPos
  f <- atom
  args <- many atom
  return (foldl (SApp i) f args)

atom :: P SNTerm
atom = oneOf [SConst <$> getPos <*> const, SV <$> getPos <*> var, parens expr, printOp]

lam :: P SNTerm
lam = do
  i <- getPos
  reserved "fun"
  bs <- many1 $ parens binding
  reservedOp "->"
  t <- expr
  return (SLam i bs t)

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

program :: P [Decl SNTerm]
program = many decl

decl :: P (Decl SNTerm)
decl = do
  i <- getPos
  reserved "let"
  v <- var
  reservedOp "="
  t <- expr
  return (Decl i v t)

declOrTm :: P (Either (Decl SNTerm) SNTerm)
declOrTm = oneOf [Left <$> decl <* eof, Right <$> expr <* eof]

-- Corre un parser, chequeando que se pueda consumir toda la entrada
runP :: P a -> String -> String -> Either ParseError a
runP p s filename = runParser (whiteSpace *> p <* eof) () filename s

--para debugging en uso interactivo (ghci)
parse :: String -> SNTerm
parse s = case runP expr s "" of
  Right t -> t
  Left e  -> error ("no parse: " ++ show s)
