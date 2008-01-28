module Compiler where

import qualified Data.ByteString.Lazy.Char8 as ByteString
import Control.Monad.Error
import System.IO

import Parser
import Pretty
import Typing
import Representation
import Library
import Interpreter
import Dataflow
import Emit

-- Functions shared between the standalone compiler and the C interface.

compile :: ShaderKind -> ByteString.ByteString -> Either String (Expr, Type, Value, ShaderInputOutput, DFGraph)
compile sk bs = do
  let (gamma, env, vrefs) = library
  (e, vrefs') <- parseExpr vrefs bs
  (t, vrefs'') <- inferExprType gamma e vrefs'
  (v, si) <- interpretExprAsShader sk env e t
  return (e, t, v, si, dependencyGraph v)

compileAndPrint :: ShaderKind -> ByteString.ByteString -> IO ()
compileAndPrint sk bs = do
  case compile sk bs of
    Right (e, t, v, si, g) -> do
      putStrLn $ prettyType t
      putStrLn $ "inputs: " ++ show si
      putStrLn "outputting graphviz..."
      hFlush stdout
      success <- graphvizCompile g (show sk) "png"
      putStrLn $ show success
      putStrLn $ emit sk si g
    Left msg -> putStrLn msg
