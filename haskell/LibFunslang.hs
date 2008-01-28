{-# OPTIONS -ffi #-}
module LibFunslang where

import qualified Data.ByteString.Lazy.Char8 as ByteString
import System.IO
import Foreign
import Foreign.C

import System.Directory
import Representation
import Emit
import Compiler

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
