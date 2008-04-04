-- Shared between the standalone compiler (Main) and the C interface (LibFunslang).
module Compiler(compile, evaluate) where

import qualified Data.ByteString.Lazy.Char8 as ByteString
import Control.Monad.Error

import Parser
import Typing
import Representation
import Library
import Interpreter
import Dataflow
import Emit
import CompileError


-- Takes a list of types (as strings) with which to unify the given type.
-- Tries them in order, and if successful returns the unified type. Otherwise,
-- fails with error message.
attemptUnification :: Type -> ([TypeVarRef], [DimVarRef]) -> [String] -> Maybe (Type, ([TypeVarRef], [DimVarRef]))
attemptUnification t1 vrefs type_strs = attemptUnification' t1 vrefs type_strs

attemptUnification' :: Type -> ([TypeVarRef], [DimVarRef]) -> [String] -> Maybe (Type, ([TypeVarRef], [DimVarRef]))
attemptUnification' _ _ [] = Nothing
attemptUnification' t1 vrefs (type_str:type_strs_left) =
  case parseType vrefs (ByteString.pack type_str) of
    Right (t2, vrefs') ->
      case runTI (mgu t1 t2) vrefs' of
        Right (s, vrefs'') -> Just (applySubstType s t1, vrefs'')
        Left _ -> attemptUnification' t1 vrefs type_strs_left
    Left err -> error $ "could not parse reference type string <" ++ type_str ++ ">!\n" ++ getErrorString err


-- Compiles a Funslang program to GLSL.
compile :: ByteString.ByteString -> ByteString.ByteString -> Either CompileError (Type, InterpretState, DFGraph, String, Type, InterpretState, DFGraph, String)
compile vertex_src fragment_src = do
  
  -- Init the library.
  let (gamma, env, var_refs_1) = library
  
  -- Parse vertex shader and infer type.
  (vertex_expr, var_refs_2) <- parseExpr var_refs_1 vertex_src
  (vertex_type, var_refs_3) <- inferExprType gamma vertex_expr var_refs_2
  
  -- Unify type with expected type.
  case attemptUnification vertex_type var_refs_3 ["a -> b -> c -> (Real 4, d)"] of
    Nothing -> throwError $ ShaderError ShaderKindVertex $ ShaderErrorBadShaderType vertex_type
    Just (vertex_type', var_refs_4) -> do
      
      -- Parse fragment shader and infer type.
      (fragment_expr, var_refs_5) <- parseExpr var_refs_4 fragment_src
      (fragment_type, var_refs_6) <- inferExprType gamma fragment_expr var_refs_5
      
      -- Unify type with expected type.
      case attemptUnification fragment_type var_refs_6 ["a -> b -> c -> Real 4", "a -> b -> c -> (Bool, Real 4)"] of
        Nothing -> throwError $ ShaderError ShaderKindFragment $ ShaderErrorBadShaderType fragment_type
        Just (fragment_type', var_refs_7) -> do
          
          -- Unify vertex shader output type with fragment shader varying type.
          let TypeFun _ (TypeFun _ (TypeFun _ (TypeTuple [_, vertex_output_type]))) = vertex_type'
          let TypeFun _ (TypeFun _ (TypeFun fragment_varying_type _)) = fragment_type'
          case runTI (mgu vertex_output_type fragment_varying_type) var_refs_7 of
            Left (TypeError _ (TypeErrorCouldNotUnify t1 t2)) -> throwError $ ShaderError ShaderKindFragment $ ShaderErrorCouldNotLink t1 t2
            Left _ -> undefined
            Right (s, _) -> do
              let vertex_type'' = applySubstType s vertex_type'
              let fragment_type'' = applySubstType s fragment_type'
              
              -- Interpret shaders to dataflow graph form.
              (vertex_value, vertex_info) <- runInterpretM (interpretExprAsShader ShaderKindVertex env vertex_expr vertex_type'') initInterpretState
              let vertex_graph = dependencyGraph vertex_value vertex_info
              (fragment_value, fragment_info) <- runInterpretM (interpretExprAsShader ShaderKindFragment env fragment_expr fragment_type'') initInterpretState{num_textures = num_textures vertex_info, textures = textures vertex_info}
              let fragment_graph = dependencyGraph fragment_value fragment_info
              
              -- Emit code.
              vertex_code <- emit ShaderKindVertex vertex_info vertex_graph
              fragment_code <- emit ShaderKindFragment fragment_info fragment_graph
              
              return (
                vertex_type'', vertex_info, vertex_graph, vertex_code,
                fragment_type'', fragment_info, fragment_graph, fragment_code
                )


-- Evaluates a debugging command (either an expression or a binding).
evaluate :: Library -> ByteString.ByteString -> Either CompileError CommandResult
evaluate (gamma, env, var_refs_1) src = do
  -- Parse command.
  (cmd, var_refs_2) <- parseCommand var_refs_1 src
  evaluate' (gamma, env, var_refs_2) cmd

evaluate' :: Library -> Command -> Either CompileError CommandResult
evaluate' (gamma, env, var_refs_2) cmd = do
  case cmd of
    CommandExpr e -> do
      -- Reinterpret as a binding to "it".
      evaluate' (gamma, env, var_refs_2) (CommandLet (PattVar "it" Nothing) e)
    
    CommandLet p e -> do
      -- Check binding type.
      (t, gamma', var_refs_3) <- inferNewEnv gamma p e var_refs_2
      -- Do binding.
      ((value, env'), info) <- runInterpretM (interpretBinding env p e) initInterpretState
      return $ CommandResult (gamma', env', var_refs_3) t value info
