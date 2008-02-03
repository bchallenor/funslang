-- Standalone compiler.
module Main where

import qualified Data.ByteString.Lazy.Char8 as ByteString
import System.IO

import System.Environment
import Representation
import Dataflow
import Compiler
import Pretty


test :: IO ()
test = withArgs ["test"] main

main :: IO ()
main = do
  a:_ <- getArgs
  
  let vpath = a ++ ".vp"
  let fpath = a ++ ".fp"
  
  vsrc <- ByteString.readFile vpath
  fsrc <- ByteString.readFile fpath
  
  case compile vsrc fsrc of
    Right (vt, vsi, vg, vemit, ft, fsi, fg, femit) -> do
      dump ShaderKindVertex vt vsi vg vemit
      dump ShaderKindFragment ft fsi fg femit
    Left msg -> putStrLn msg

dump :: ShaderKind -> Type -> ShaderState -> DFGraph -> String -> IO ()
dump sk t si g emitted = do
  putStrLn $ prettyType t
  putStrLn $ "inputs: " ++ show si
  putStrLn "outputting graphviz..."
  hFlush stdout
  success <- graphvizCompile g (show sk) "png"
  putStrLn $ show success
  putStrLn $ emitted