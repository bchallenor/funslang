module Main where

import qualified Data.ByteString.Lazy.Char8 as ByteString
import Control.Monad.Error
import System.IO

import System.Directory
import System.Environment
import Representation
import Compiler

-- Standalone compiler.

test :: IO ()
test = withArgs ["test"] main

main :: IO ()
main = do
  a:_ <- getArgs
  
  exists_vp <- doesFileExist $ a ++ ".vp"
  if exists_vp
    then do
      putStrLn "vertex..."
      bsvp <- ByteString.readFile $ a ++ ".vp"
      compileAndPrint ShaderKindVertex bsvp
    else return ()
  
  exists_fp <- doesFileExist $ a ++ ".fp"
  if exists_fp
    then do
      putStrLn "fragment..."
      bsfp <- ByteString.readFile $ a ++ ".fp"
      compileAndPrint ShaderKindFragment bsfp
    else return ()
