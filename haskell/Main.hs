module Main where

import qualified Data.ByteString.Lazy.Char8 as ByteString
import Control.Monad.Error

import Parser
import System.Environment
import Pretty
import Typing
import Representation
import Library
import Interpreter

test :: IO ()
test = withArgs ["test.vp"] main

compile :: ByteString.ByteString -> Either String (Expr, Type, Value, Value)
compile bs = do
  let (gamma, vrefs) = libraryTypeSchemes
  (e, vrefs') <- parseExpr vrefs bs
  t <- inferExprType gamma e vrefs'
  v1 <- interpretExpr libraryValues e
  v2 <- interpretExprAsShader libraryValues e t
  return (e, t, v1, v2)

main :: IO ()
main = do
  a:_ <- getArgs
  bs <- ByteString.readFile a
  case compile bs of
    Right (e, t, v1, v2) -> putStrLn $ prettyExpr e ++ "\n\n" ++ prettyType t ++ "\n\n" ++ show v1 ++ "\n\n" ++ show v2
    Left msg -> putStrLn msg
