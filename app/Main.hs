{-# LANGUAGE RecordWildCards #-}

-- |
-- Module      : Main
-- Description : Compilador de FD4.
-- Copyright   : (c) Mauro Jaskelioff, Guido Martínez, 2020.
-- License     : GPL-3
-- Maintainer  : mauro@fceia.unr.edu.ar
-- Stability   : experimental
module Main where

import Bytecompile (bcRead, bcWrite, bytecompile, runBC)
import C
import CEK (evalCEK)
import ClosureConvert
import Control.Exception (IOException, catch)
import Control.Monad.Catch (MonadMask)
import Control.Monad.Trans
import Data.Char (isSpace)
import Data.List (intersperse, isPrefixOf, nub)
import Data.Maybe
import Elab (elab, elabDecl)
import Errors
import Eval (eval)
import Global (GlEnv (..))
import IR
import Lang
import MonadFD4
import Optimize (optimize, optimizeDecl)
import Options.Applicative
import PPrint (pp, ppTy, ppDecl, ppDull)
import Parse (P, declOrTm, program, runP, tm)
import Subst
import System.Console.Haskeline
  ( InputT,
    defaultSettings,
    getInputLine,
    runInputT,
  )
import System.Exit
import System.FilePath (splitExtension)
import System.IO (hPrint, hPutStrLn, stderr)
import TypeChecker (tcTerm, tcDecl)

prompt :: String
prompt = "FD4> "

{-
 Tipo para representar las banderas disponibles en línea de comando.
-}
data Mode
  = Interactive
  | Run
  | Typecheck
  | InteractiveCEK
  | RunCEK
  | Bytecompile
  | RunVM
  | CC
  deriving (Eq)

-- | Canon
-- | LLVM
-- | Build

-- | Parser de banderas
parseMode :: Parser (Mode, Bool)
parseMode =
  (,)
    <$> ( flag' Typecheck (long "typecheck" <> short 't' <> help "Chequear tipos e imprimir el término")
            <|> flag' InteractiveCEK (long "interactiveCEK" <> short 'k' <> help "Ejecutar interactivamente en la CEK")
            <|> flag' RunCEK (long "runCEK" <> help "Ejecutar en la CEK")
            <|> flag' Bytecompile (long "bytecompile" <> short 'm' <> help "Compilar a la BVM")
            <|> flag' RunVM (long "runVM" <> short 'r' <> help "Ejecutar bytecode en la BVM")
            <|> flag Interactive Interactive (long "interactive" <> short 'i' <> help "Ejecutar en forma interactiva")
            <|> flag' Run (long "run" <> help "Ejecutar")
            <|> flag' CC (long "cc" <> short 'c' <> help "Compilar a código C")
            -- <|> flag' Canon ( long "canon" <> short 'n' <> help "Imprimir canonicalización")
            -- <|> flag' LLVM ( long "llvm" <> short 'l' <> help "Imprimir LLVM resultante")
            -- <|> flag' Build ( long "build" <> short 'b' <> help "Compilar")
        )
    <*> flag False True (long "optimize" <> short 'o' <> help "Optimizar código")

-- | Parser de opciones general, consiste de un modo y una lista de archivos a procesar
parseArgs :: Parser (Mode, Bool, [FilePath])
parseArgs = (\(a, b) c -> (a, b, c)) <$> parseMode <*> many (argument str (metavar "FILES..."))

main :: IO ()
main = execParser opts >>= go
  where
    opts =
      info
        (parseArgs <**> helper)
        ( fullDesc
            <> progDesc "Compilador de FD4"
            <> header "Compilador de FD4 de la materia Compiladores 2021"
        )

    go :: (Mode, Bool, [FilePath]) -> IO ()
    go (Interactive, opt, files) =
      do
        runFD4 (runInputT defaultSettings (repl files Interactive opt))
        return ()
    go (Run, opt, files) =
      do
        runFD4 $ compileAndRun files Run opt
        return ()
    go (Typecheck, opt, files) =
      runOrFail $ mapM_ (typecheckFile opt) files
    go (InteractiveCEK, opt, files) =
      do
        runFD4 (runInputT defaultSettings (repl files InteractiveCEK opt))
        return ()
    go (RunCEK, opt, files) =
      do
        runFD4 $ compileAndRun files RunCEK opt
        return ()
    go (Bytecompile, opt, files) =
      runOrFail $ mapM_ (bytecompileFile opt) files
    go (RunVM, _, files) =
      runOrFail $ mapM_ bytecodeRun files
    go (CC, opt, files) =
      runOrFail $ mapM_ (ccFile opt) files

-- go (Canon,_, files) =
--           runOrFail $ mapM_ canonFile files
-- go (LLVM,_, files) =
--           runOrFail $ mapM_ llvmFile files
-- go (Build,_, files) =
--           runOrFail $ mapM_ buildFile files

compileAndRun :: MonadFD4 m => [FilePath] -> Mode -> Bool -> m ()
compileAndRun files mode opt = do
  compileFiles opt files
  mDecl <- getLastDecl
  case mDecl of
    Nothing -> return ()
    Just (Decl _ _ _ t) -> handleTerm t mode opt

runOrFail :: FD4 a -> IO a
runOrFail m = do
  r <- runFD4 m
  case r of
    Left err -> do
      liftIO $ hPrint stderr err
      exitWith (ExitFailure 1)
    Right v -> return v

repl :: (MonadFD4 m, MonadMask m) => [FilePath] -> Mode -> Bool -> InputT m ()
repl args mode opt = do
  lift $ catchErrors $ compileFiles opt args
  s <- lift get
  when (inter s) $
    liftIO $
      putStrLn
        ( "Entorno interactivo para FD4.\n"
            ++ "Escriba :? para recibir ayuda."
        )
  loop
  where
    loop = do
      minput <- getInputLine prompt
      case minput of
        Nothing -> return ()
        Just "" -> loop
        Just x -> do
          c <- liftIO $ interpretCommand x
          b <- lift $ catchErrors $ handleCommand c mode opt
          maybe loop (`when` loop) b

compileFiles :: MonadFD4 m => Bool -> [FilePath] -> m ()
compileFiles _ [] = return ()
compileFiles opt (x : xs) = do
  modify (\s -> s {lfile = x, inter = False})
  compileFile opt x
  compileFiles opt xs

loadFile :: MonadFD4 m => FilePath -> m [SDecl]
loadFile f = do
  let filename = reverse (dropWhile isSpace (reverse f))
  x <-
    liftIO $
      catch
        (readFile filename)
        ( \e -> do
            let err = show (e :: IOException)
            hPutStrLn stderr ("No se pudo abrir el archivo " ++ filename ++ ": " ++ err)
            return ""
        )
  setLastFile filename
  parseIO filename program x

compileFile :: MonadFD4 m => Bool -> FilePath -> m ()
compileFile opt f = do
  decls <- loadFile f
  mapM_ (handleSDecl opt) decls

typecheckFile :: MonadFD4 m => Bool -> FilePath -> m ()
typecheckFile opt f = do
  decls <- loadFile f
  decls' <- mapM (handleSDecl opt) decls
  mapM_ (printFD4 <=< ppDecl) (catMaybes decls')

declsToTerm :: [DeclTerm] -> Term
declsToTerm [] = error "No main function"
declsToTerm [Decl _ _ _ t] = t
declsToTerm (Decl p n ty t : ds) = Let p n ty t (close n (glob2free (declsToTerm ds)))

bytecompileFile :: MonadFD4 m => Bool -> FilePath -> m ()
bytecompileFile opt f = do
  decls <- loadFile f
  mdecls <- mapM elabDecl decls
  let decls' = catMaybes mdecls
  let term = declsToTerm decls'
  tcTerm term
  t <- if opt then optimize term else return term
  bytecode <- bytecompile t
  liftIO $ bcWrite bytecode (fst (splitExtension f) ++ ".byte")

bytecodeRun :: MonadFD4 m => FilePath -> m ()
bytecodeRun = liftIO . bcRead >=> runBC

ccFile :: MonadFD4 m => Bool -> FilePath -> m ()
ccFile opt f = do
  decls <- loadFile f
  mdecls <- mapM elabDecl decls
  let decls' = catMaybes mdecls
  mapM_ tcAddTy decls'
  decls'' <- if opt then mapM optimizeDecl decls' else return decls'
  let irdecls = runCC decls''
  liftIO $ writeFile (fst (splitExtension f) ++ ".c") (ir2C (IrDecls irdecls))
  where
    tcAddTy :: MonadFD4 m => DeclTerm -> m ()
    tcAddTy decl = do
      ty <- tcDecl decl
      addTy (declName decl) ty

parseIO :: MonadFD4 m => String -> P a -> String -> m a
parseIO filename p x = case runP p x filename of
  Left e -> throwError (ParseErr e)
  Right r -> return r

handleSDecl :: MonadFD4 m => Bool -> SDecl -> m (Maybe DeclTerm)
handleSDecl opt d = do
  md <- elabDecl d
  case md of
    Nothing -> return Nothing
    Just d' -> do
      ty <- tcDecl d'
      d'' <- if opt then optimizeDecl d' else return d'
      addTy (declName d'') ty
      addDecl d''
      return $ Just d''

handleSDecl_ :: MonadFD4 m => Bool -> SDecl -> m ()
handleSDecl_ opt d = void (handleSDecl opt d)

data Command
  = Compile CompileForm
  | PPrint String
  | Type String
  | Reload
  | Browse
  | Quit
  | Help
  | Noop

data CompileForm
  = CompileInteractive String
  | CompileFile String

data InteractiveCommand = Cmd [String] String (String -> Command) String

-- | Parser simple de comando interactivos
interpretCommand :: String -> IO Command
interpretCommand x =
  if ":" `isPrefixOf` x
    then do
      let (cmd, t') = break isSpace x
          t = dropWhile isSpace t'
      --  find matching commands
      let matching = filter (\(Cmd cs _ _ _) -> any (isPrefixOf cmd) cs) commands
      case matching of
        [] -> do
          putStrLn ("Comando desconocido `" ++ cmd ++ "'. Escriba :? para recibir ayuda.")
          return Noop
        [Cmd _ _ f _] ->
          do return (f t)
        _ -> do
          putStrLn
            ( "Comando ambigüo, podría ser "
                ++ concat (intersperse ", " [head cs | Cmd cs _ _ _ <- matching])
                ++ "."
            )
          return Noop
    else return (Compile (CompileInteractive x))

commands :: [InteractiveCommand]
commands =
  [ Cmd [":browse"] "" (const Browse) "Ver los nombres en scope",
    Cmd
      [":load"]
      "<file>"
      (Compile . CompileFile)
      "Cargar un programa desde un archivo",
    Cmd [":print"] "<exp>" PPrint "Imprime un término y sus ASTs sin evaluarlo",
    Cmd [":reload"] "" (const Reload) "Vuelve a cargar el último archivo cargado",
    Cmd [":type"] "<exp>" Type "Chequea el tipo de una expresión",
    Cmd [":quit", ":Q"] "" (const Quit) "Salir del intérprete",
    Cmd [":help", ":?"] "" (const Help) "Mostrar esta lista de comandos"
  ]

helpTxt :: [InteractiveCommand] -> String
helpTxt cs =
  "Lista de comandos:  Cualquier comando puede ser abreviado a :c donde\n"
    ++ "c es el primer caracter del nombre completo.\n\n"
    ++ "<expr>                  evaluar la expresión\n"
    ++ "let <var> = <expr>      definir una variable\n"
    ++ unlines
      ( map
          ( \(Cmd c a _ d) ->
              let ct = concat (intersperse ", " (map (++ if null a then "" else " " ++ a) c))
               in ct ++ replicate ((24 - length ct) `max` 2) ' ' ++ d
          )
          cs
      )

-- | 'handleCommand' interpreta un comando y devuelve un booleano
-- indicando si se debe salir del programa o no.
handleCommand :: MonadFD4 m => Command -> Mode -> Bool -> m Bool
handleCommand cmd mode opt = do
  s@GlEnv {..} <- get
  case cmd of
    Quit -> return False
    Noop -> return True
    Help -> printFD4 (helpTxt commands) >> return True
    Browse -> do
      printFD4 (unlines [name | name <- reverse (nub (map declName glb))])
      printFD4 (unlines [name | name <- reverse (nub (map fst tyEnv))])
      return True
    Compile c ->
      do
        case c of
          CompileInteractive e -> compilePhrase e mode opt
          CompileFile f -> put (s {lfile = f, cantDecl = 0}) >> compileFile opt f
        return True
    Reload -> eraseLastFileDecls >> (getLastFile >>= compileFile opt) >> return True
    PPrint e -> printPhrase e >> return True
    Type e -> typeCheckPhrase e >> return True

compilePhrase :: MonadFD4 m => String -> Mode -> Bool -> m ()
compilePhrase x mode opt =
  do
    dot <- parseIO "<interactive>" declOrTm x
    case dot of
      Left d -> handleSDecl_ opt d
      Right t -> handleSNTerm t mode opt

handleSNTerm :: MonadFD4 m => SNTerm -> Mode -> Bool -> m ()
handleSNTerm t mode opt = do
  t' <- elab t
  handleTerm t' mode opt

handleTerm :: MonadFD4 m => Term -> Mode -> Bool -> m ()
handleTerm t mode opt = do
  ty <- tcTerm t
  t' <- if opt then optimize t else return t
  te <- evalTerm mode t'
  displayResult mode te ty
  where
    evalTerm :: MonadFD4 m => Mode -> Term -> m Term
    evalTerm Interactive = eval
    evalTerm Run = eval
    evalTerm InteractiveCEK = evalCEK
    evalTerm RunCEK = evalCEK
    evalTerm _ = undefined

    displayResult :: MonadFD4 m => Mode -> Term -> Ty -> m ()
    displayResult m te ty | m == Run || m == RunCEK = do
      ppte <- ppDull te
      printFD4 ppte
    displayResult m te ty | m == Interactive || m == InteractiveCEK = do
      ppte <- pp te
      printFD4 $ ppte ++ " : " ++ ppTy ty
    displayResult _ _ _ = undefined

printPhrase :: MonadFD4 m => String -> m ()
printPhrase phrase =
  do
    t <- parseIO "<interactive>" tm phrase
    et <- elab t
    t' <- case t of
      (SV p f) -> fromMaybe et <$> maybeGetDecl f
      _ -> return et
    printFD4 "SNTerm:"
    printFD4 (show t)
    printFD4 "\nTerm:"
    printFD4 (show t')

typeCheckPhrase :: MonadFD4 m => String -> m ()
typeCheckPhrase x = do
  t <- parseIO "<interactive>" tm x
  t' <- elab t
  ty <- tcTerm t'
  printFD4 (ppTy ty)
