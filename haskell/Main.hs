module Main where

import qualified Data.ByteString.Lazy.Char8 as ByteString
import qualified Data.Map as Map
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

compile :: ByteString.ByteString -> Either String (Expr, Type, Value)
compile bs = do
  let (gamma, vrefs) = libraryTypeSchemes
  (e, vrefs') <- parseExpr vrefs bs
  t <- inferExprType gamma e vrefs'
  v <- interpretExpr libraryValues e
  return (e, t, v)

main :: IO ()
main = do
  a:_ <- getArgs
  bs <- ByteString.readFile a
  case compile bs of
    Right (e, t, v) -> putStrLn $ prettyExpr e ++ "\n\n" ++ prettyType t ++ "\n\n" ++ show v
    Left msg -> putStrLn msg
