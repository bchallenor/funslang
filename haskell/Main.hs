{-# OPTIONS -ffi #-}
module Main where

import qualified Data.ByteString.Lazy.Char8 as ByteString
import Control.Monad.Error
import System.IO
import Foreign
import Foreign.C

import Parser
import System.Directory
import System.Environment
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


-- Functions for use as a standalone compiler.

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

test :: IO ()
test = withArgs ["test"] main


-- The following functions export the compiler to C.

fsCompileFunslang :: ShaderKind -> CString -> Ptr Bool -> Ptr CInt -> Ptr CInt -> Ptr CString -> IO ()
fsCompileFunslang sk path_cstr success_ptr num_uniforms_ptr num_varyings_ptr emit_cstr_ptr = do
  path <- peekCString path_cstr
  exists <- doesFileExist path
  if exists
    then do
      bs <- ByteString.readFile path
      case compile sk bs of
        Right (_, _, _, si, g) -> do
          emit_cstr <- newCString $ emit sk si g
          poke success_ptr True
          poke num_uniforms_ptr $ fromIntegral $ num_uniforms si
          poke num_varyings_ptr $ fromIntegral $ num_varyings si
          poke emit_cstr_ptr emit_cstr
        Left msg -> do
          msg_cstr <- newCString msg
          poke success_ptr False
          poke emit_cstr_ptr msg_cstr
    else do
      msg_cstr <- newCString $ "file <" ++ path ++ "> does not exist"
      poke success_ptr False
      poke emit_cstr_ptr msg_cstr

fsCompileFunslangVertex :: CString -> Ptr Bool -> Ptr CInt -> Ptr CInt -> Ptr CString -> IO ()
fsCompileFunslangVertex = fsCompileFunslang ShaderKindVertex

fsCompileFunslangFragment :: CString -> Ptr Bool -> Ptr CInt -> Ptr CInt -> Ptr CString -> IO ()
fsCompileFunslangFragment = fsCompileFunslang ShaderKindFragment

fsCompileFree :: CString -> IO ()
fsCompileFree cstr = free cstr

foreign export ccall "fsCompileFunslangVertex" fsCompileFunslangVertex :: CString -> Ptr Bool -> Ptr CInt -> Ptr CInt -> Ptr CString -> IO ()
foreign export ccall "fsCompileFunslangFragment" fsCompileFunslangFragment :: CString -> Ptr Bool -> Ptr CInt -> Ptr CInt -> Ptr CString -> IO ()
foreign export ccall "fsCompileFree" fsCompileFree :: CString -> IO ()
