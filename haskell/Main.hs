import Lexer
import Parser
import System.Environment
import Pretty
import Typing
import Representation

test :: IO ()
test = withArgs ["test.vp"] main

ioe :: IO Expr
ioe = do
  s <- readFile "test.vp"
  return ((parser . lexer) s)

ioe' :: IO TypedExpr
ioe' = do
  e <- ioe
  e' <- inferType e
  return e'

iot :: IO Type
iot = do
  e' <- ioe'
  return (typeOf e')

main :: IO ()
main = do
  a:_ <- getArgs
  s <- readFile a
  let e = (parser . lexer) s
  putStrLn (show e)
  putStrLn ""
  putStrLn (prettyExpr e)
  putStrLn ""
  putStrLn (case inferType e of Typeable e -> prettyType (typeOf e); Untypeable s -> s)
