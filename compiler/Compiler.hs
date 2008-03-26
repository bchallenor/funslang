-- Shared between the standalone compiler (Main) and the C interface (LibFunslang).
module Compiler(compile, evaluate) where

import qualified Data.ByteString.Lazy.Char8 as ByteString
import Control.Monad.Error

import Parser
import Pretty
import Typing
import Representation
import Library
import Interpreter
import Dataflow
import Emit


-- Takes a list of types (as strings) with which to unify the given type.
-- Tries them in order, and if successful returns the unified type. Otherwise,
-- fails with error message.
attemptUnification :: Type -> ([TypeVarRef], [DimVarRef]) -> [String] -> Either String (Type, ([TypeVarRef], [DimVarRef]))
attemptUnification t1 vrefs type_strs = attemptUnification' t1 vrefs type_strs type_strs

attemptUnification' :: Type -> ([TypeVarRef], [DimVarRef]) -> [String] -> [String] -> Either String (Type, ([TypeVarRef], [DimVarRef]))
attemptUnification' t1 _ type_strs [] =
  Left $ "could not unify <" ++ prettyType t1 ++ "> with any of " ++ array type_strs
attemptUnification' t1 vrefs type_strs (type_str:type_strs_left) =
  case parseType vrefs (ByteString.pack type_str) of
    Right (t2, vrefs') ->
      case runTI (mgu t1 t2) vrefs' of
        Right (s, vrefs'') -> Right (applySubstType s t1, vrefs'')
        Left _ -> attemptUnification' t1 vrefs type_strs type_strs_left
    Left msg -> Left $ "could not parse type: " ++ type_str ++ "\n" ++ msg


-- Compiles a Funslang program to GLSL.
compile :: ByteString.ByteString -> ByteString.ByteString -> Either String (Type, InterpretState, DFGraph, String, Type, InterpretState, DFGraph, String)
compile vertex_src fragment_src = do
  
  -- Init the library.
  let (gamma, env, var_refs_1) = library
  
  -- Parse vertex shader and infer type.
  (vertex_expr, var_refs_2) <- parseExpr var_refs_1 vertex_src
  (vertex_type, var_refs_3) <- inferExprType gamma vertex_expr var_refs_2
  
  -- Unify type with expected type.
  case attemptUnification vertex_type var_refs_3 ["a -> b -> c -> (Real 4, d)"] of
    Left msg -> Left $ "vertex shader has incorrect type:\n" ++ msg
    Right (vertex_type', var_refs_4) -> do
      
      -- Parse fragment shader and infer type.
      (fragment_expr, var_refs_5) <- parseExpr var_refs_4 fragment_src
      (fragment_type, var_refs_6) <- inferExprType gamma fragment_expr var_refs_5
      
      -- Unify type with expected type.
      case attemptUnification fragment_type var_refs_6 ["a -> b -> c -> Real 4", "a -> b -> c -> (Bool, Real 4)"] of
        Left msg -> Left $ "fragment shader has incorrect type:\n" ++ msg
        Right (fragment_type', var_refs_7) -> do
          
          -- Unify vertex shader output type with fragment shader varying type.
          let TypeFun _ (TypeFun _ (TypeFun _ (TypeTuple [_, vertex_output_type]))) = vertex_type'
          let TypeFun _ (TypeFun _ (TypeFun fragment_varying_type _)) = fragment_type'
          case runTI (mgu vertex_output_type fragment_varying_type) var_refs_7 of
            Left msg -> Left $ "could not unify vertex shader output type with fragment shader input type:\n" ++ msg
            Right (s, _) -> do
              let vertex_type'' = applySubstType s vertex_type'
              let fragment_type'' = applySubstType s fragment_type'
              
              -- Interpret shaders to dataflow graph form.
              (vertex_value, vertex_info) <- runInterpretM (interpretExprAsShader ShaderKindVertex env vertex_expr vertex_type'') initInterpretState
              let vertex_graph = dependencyGraph vertex_value vertex_info
              (fragment_value, fragment_info) <- runInterpretM (interpretExprAsShader ShaderKindFragment env fragment_expr fragment_type'') initInterpretState{num_textures = num_textures vertex_info, textures = textures vertex_info}
              let fragment_graph = dependencyGraph fragment_value fragment_info
              
              return (
                vertex_type'', vertex_info, vertex_graph, emit ShaderKindVertex vertex_info vertex_graph,
                fragment_type'', fragment_info, fragment_graph, emit ShaderKindFragment fragment_info fragment_graph
                )


-- Evaluates a debugging command (either an expression or a binding).
evaluate :: Library -> ByteString.ByteString -> Either String CommandResult
evaluate (gamma, env, var_refs_1) src = do
  -- Parse command.
  (cmd, var_refs_2) <- parseCommand var_refs_1 src
  evaluate' (gamma, env, var_refs_2) cmd

evaluate' :: Library -> Command -> Either String CommandResult
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
