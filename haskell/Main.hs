module Main where

import qualified Data.ByteString.Lazy.Char8 as ByteString

import Parser
import System.Environment
import Pretty
import Typing
import Representation
import Library

test :: IO ()
test = withArgs ["test.vp"] main

-- ioe' :: IO TypedExpr
-- ioe' = do
--   e <- ioe
--   e' <- inferType e
--   return e'

-- iot :: IO Type
-- iot = do
--   e' <- ioe'
--   return (typeOf e')

-- unify :: String -> String -> Either String Subst
-- unify tstr1 tstr2 = do
--   let xt1 = parseExType $ lexer tstr1
--   let xt2 = parseExType $ lexer tstr2
--   let ([t1, t2], _) = typesFromExTypes initFreshVarRefs [xt1, xt2]
--   runTI (mgu t1 t2) ([], [])

ti :: Expr -> ([TypeVarRef], [DimVarRef]) -> String
ti e vrefs =
  let (gamma, _) = initLibrary in
  case runTI (principalType gamma e) vrefs of
    Right (s, t) -> show s ++ "\n\n" ++ show t ++ "\n\n" ++ prettyType t
    Left s -> s

main :: IO ()
main = do
  a:_ <- getArgs
  s <- ByteString.readFile a
  let (_, freshVarRefs) = initLibrary
  let POk PState{ fresh_vrefs = vrefs } e = parseExpr freshVarRefs s
  putStrLn (show e)
  putStrLn ""
  putStrLn (prettyExpr e)
  putStrLn ""
  putStrLn $ ti e vrefs
