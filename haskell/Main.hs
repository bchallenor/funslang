module Main where

import Lexer
import Parser
import System.Environment
import Pretty
import Typing
import Representation
import Library

test :: IO ()
test = withArgs ["test.vp"] main

ioe :: IO Expr
ioe = do
  s <- readFile "test.vp"
  return ((parseExpr . lexer) s)

-- ioe' :: IO TypedExpr
-- ioe' = do
--   e <- ioe
--   e' <- inferType e
--   return e'

-- iot :: IO Type
-- iot = do
--   e' <- ioe'
--   return (typeOf e')

unify :: String -> String -> Either String Subst
unify tstr1 tstr2 = do
  let xt1 = parseExType $ lexer tstr1
  let xt2 = parseExType $ lexer tstr2
  let [t1, t2] = typesFromExTypes typeVarRefs dimVarRefs [xt1, xt2]
  runTI (mgu t1 t2) (typeVarRefs, dimVarRefs)

ti :: Expr -> String
ti e =
  case runTI (principalType (libraryEnv) e) (typeVarRefs, dimVarRefs) of
    Right (s, t) -> show s ++ "  " ++ show (applySubst s t) ++ "  " ++ prettyType (applySubst s t)
    Left s -> s

main :: IO ()
main = do
  a:_ <- getArgs
  s <- readFile a
  let e = (parseExpr . lexer) s
  putStrLn (show e)
  putStrLn ""
  putStrLn (prettyExpr e)
  putStrLn ""
  putStrLn $ ti e
