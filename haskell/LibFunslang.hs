{-# OPTIONS -ffi #-}

-- Exports the compiler to C.
module LibFunslang where

import qualified Data.ByteString.Lazy.Char8 as ByteString
import System.IO
import Foreign
import Foreign.C

import System.Directory
import Representation
import Pretty
import Compiler


fsCompile :: CString -> CString -> Ptr CString -> Ptr CString -> Ptr CInt -> Ptr CInt -> Ptr CString -> Ptr CString -> Ptr CInt -> Ptr CInt -> Ptr CString -> Ptr CInt -> IO ()
fsCompile v_path_cstr f_path_cstr err_cstr_ptr v_type_cstr_ptr v_num_uniforms_ptr v_num_varyings_ptr v_emit_cstr_ptr f_type_cstr_ptr f_num_uniforms_ptr f_num_varyings_ptr f_emit_cstr_ptr num_textures_ptr = do
  v_path <- peekCString v_path_cstr
  f_path <- peekCString f_path_cstr
  
  v_exists <- doesFileExist v_path
  f_exists <- doesFileExist f_path
  
  if not v_exists || not f_exists
    then do
      err_cstr <- newCString "file does not exist"
      poke err_cstr_ptr err_cstr
      
    else do
      v_h <- openFile v_path ReadMode
      v_src <- ByteString.hGetContents v_h
      
      f_h <- openFile f_path ReadMode
      f_src <- ByteString.hGetContents f_h
      
      case compile v_src f_src of
        Right (v_type, v_si, _, v_emit, f_type, f_si, _, f_emit) -> do
          
          poke v_num_uniforms_ptr $ fromIntegral $ num_uniforms v_si
          poke v_num_varyings_ptr $ fromIntegral $ num_varyings v_si
          v_emit_cstr <- newCString v_emit
          poke v_emit_cstr_ptr v_emit_cstr
          v_type_cstr <- newCString $ prettyType v_type
          poke v_type_cstr_ptr v_type_cstr
          
          poke f_num_uniforms_ptr $ fromIntegral $ num_uniforms f_si
          poke f_num_varyings_ptr $ fromIntegral $ num_varyings f_si
          f_emit_cstr <- newCString f_emit
          poke f_emit_cstr_ptr f_emit_cstr
          f_type_cstr <- newCString $ prettyType f_type
          poke f_type_cstr_ptr f_type_cstr
          
          poke num_textures_ptr $ fromIntegral $ num_textures f_si
          
          poke err_cstr_ptr nullPtr
          
          hClose v_h
          hClose f_h
          
        Left msg -> do
          
          err_cstr <- newCString msg
          poke err_cstr_ptr err_cstr
          
          hClose v_h
          hClose f_h


foreign export ccall "_fsCompile" fsCompile :: CString -> CString -> Ptr CString -> Ptr CString -> Ptr CInt -> Ptr CInt -> Ptr CString -> Ptr CString -> Ptr CInt -> Ptr CInt -> Ptr CString -> Ptr CInt -> IO ()
foreign export ccall "_fsFree" free :: Ptr a -> IO ()
