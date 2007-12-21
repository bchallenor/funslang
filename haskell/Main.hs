module Main where

import qualified Data.ByteString.Lazy.Char8 as ByteString
import Control.Monad.Error
import System.IO

import Parser
import System.Environment
import Pretty
import Typing
import Representation
import Library
import Interpreter
import Dataflow

test :: IO ()
test = withArgs ["test.vp"] main

compile :: ByteString.ByteString -> Either String (Expr, Type, Value, Value, DFGraph)
compile bs = do
  let (gamma, env, vrefs) = library
  (e, vrefs') <- parseExpr vrefs bs
  (t, vrefs'') <- inferExprType gamma e vrefs'
  v1 <- interpretExpr env e
  v2 <- interpretExprAsShader env e t
  return (e, t, v1, v2, dependencyGraph v2)

main :: IO ()
main = do
  a:_ <- getArgs
  bs <- ByteString.readFile a
  case compile bs of
    Right (e, t, v1, v2, g) -> do
      --putStrLn $ prettyExpr e ++ "\n\n" ++ prettyType t
      putStrLn $ prettyType t
      putStrLn "outputting graphviz..."
      hFlush stdout
      success <- graphvizCompile g "graph" "png"
      putStrLn $ show success
    Left msg -> putStrLn msg
