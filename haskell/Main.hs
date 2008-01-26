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
import Emit

test :: IO ()
test = withArgs ["test.vp"] main

compile :: ByteString.ByteString -> Either String (Expr, Type, Value, ShaderNumInputs, DFGraph)
compile bs = do
  let (gamma, env, vrefs) = library
  (e, vrefs') <- parseExpr vrefs bs
  (t, vrefs'') <- inferExprType gamma e vrefs'
  (v, num_inputs) <- interpretExprAsShader env e t
  return (e, t, v, num_inputs, dependencyGraph v)

main :: IO ()
main = do
  a:_ <- getArgs
  bs <- ByteString.readFile a
  case compile bs of
    Right (e, t, v, num_inputs, g) -> do
      --putStrLn $ prettyExpr e ++ "\n\n" ++ prettyType t
      putStrLn $ prettyType t
      putStrLn $ "inputs: " ++ show num_inputs
      putStrLn "outputting graphviz..."
      hFlush stdout
      success <- graphvizCompile g "graph" "png"
      putStrLn $ show success
      putStrLn "vertex..."
      putStrLn $ emit ShaderKindVertex num_inputs g
      putStrLn "fragment..."
      putStrLn $ emit ShaderKindFragment num_inputs g
    Left msg -> putStrLn msg
