module Interpreter(interpretExpr, interpretExprAsShader, interpretBinding) where

import qualified Data.Map as Map
import qualified Data.List as List
import Control.Monad.Error
import Control.Monad.State

import Representation
import Pretty
import CompileError


-- The inner interpreter function.
-- Returns the value, or on error, a message explaining the error and the expression that triggered it.
interpretExpr :: ValueEnv -> Expr -> InterpretM Value

interpretExpr _ (ExprUnitLiteral) = return ValueUnit

interpretExpr _ (ExprRealLiteral b) = do
  n <- freshNode
  return $ ValueReal $ DFRealLiteral n b

interpretExpr _ (ExprBoolLiteral b) = do
  n <- freshNode
  return $ ValueBool $ DFBoolLiteral n b

interpretExpr env (ExprVar ident) = do
  case Map.lookup ident env of
    Just v -> v
    Nothing -> error $ "variable <" ++ ident ++ "> undefined, should have been caught by type checker!"

interpretExpr env a@(ExprApp e1 e2) = registerStackPoint a $ do
  ValueFun f <- interpretExpr env e1
  v <- interpretExpr env e2
  f v

interpretExpr env a@(ExprArray es) = registerStackPoint a $ do
  vs <- mapM (interpretExpr env) es
  return $ ValueArray vs

interpretExpr env a@(ExprTuple es) = registerStackPoint a $ do
  vs <- mapM (interpretExpr env) es
  return $ ValueTuple vs

interpretExpr env a@(ExprIf eb e1 e2) = registerStackPoint a $ do
  ValueBool dfb <- interpretExpr env eb
  v1 <- interpretExpr env e1
  v2 <- interpretExpr env e2
  if v1 == v2
    then return v1 -- optimize out if both branches are the same
    else case dfb of
      DFBoolLiteral _ b -> return $ if b then v1 else v2 -- optimize out if condition statically known
      _ -> conditionalize dfb v1 v2 -- runtime condition and runtime values

interpretExpr env a@(ExprLet p e1 e2) = registerStackPoint a $ do
  (_, env') <- interpretBinding env p e1
  v2 <- interpretExpr env' e2
  return v2

interpretExpr env a@(ExprLambda p e) = registerStackPoint a $ do
  -- it is critical that Map.union prefers its first argument
  return $ ValueFun (\v -> let env' = matchPattern p v `Map.union` env in interpretExpr env' e)


-- Binds p to e, returning the new environment produced.
-- Helper function used both by normal let expressions and by debug let commands.
interpretBinding :: ValueEnv -> Patt -> Expr -> InterpretM (Value, ValueEnv)
interpretBinding env p e =
  registerStackPoint e $ do
  v <- interpretExpr env e
  -- it is critical that Map.union prefers its first argument
  let env' = matchPattern p v `Map.union` env
  return (v, env')


-- Takes a boolean condition and zips up the two values with DFCond nodes,
-- so that the resulting value is either the first or second value according to the condition.
conditionalize :: DFBool -> Value -> Value -> InterpretM Value
conditionalize _ (ValueUnit) (ValueUnit) = return ValueUnit
conditionalize dfb (ValueReal df1) (ValueReal df2) = do
  n <- freshNode
  return $ ValueReal $ DFRealCond n dfb df1 df2
conditionalize dfb (ValueBool df1) (ValueBool df2) = do
  n <- freshNode
  return $ ValueBool $ DFBoolCond n dfb df1 df2
conditionalize dfb (ValueTex df1) (ValueTex df2) = do
  n <- freshNode
  return $ ValueTex $ DFTexCond n dfb df1 df2
conditionalize dfb (ValueArray vs1) (ValueArray vs2) = do
  vs <- zipWithM (conditionalize dfb) vs1 vs2
  return $ ValueArray vs
conditionalize dfb (ValueTuple vs1) (ValueTuple vs2) = do
  vs <- zipWithM (conditionalize dfb) vs1 vs2
  return $ ValueTuple vs
conditionalize dfb (ValueFun f1) (ValueFun f2) = return $
  ValueFun $ \ v -> do
    v1 <- f1 v
    v2 <- f2 v
    conditionalize dfb v1 v2
conditionalize _ _ _ = error "unexpected case in conditionalize"


-- Match the pattern against the value to give a value environment.
matchPattern :: Patt -> Value -> ValueEnv
matchPattern (PattWild _) _ = Map.empty
matchPattern (PattUnit _) _ = Map.empty
matchPattern (PattVar ident _) v = Map.singleton ident (return v)
matchPattern (PattArray ps _) (ValueArray vs) = List.foldl1' Map.union (zipWith matchPattern ps vs)
matchPattern (PattArray _ _) _ = undefined
matchPattern (PattTuple ps _) (ValueTuple vs) = List.foldl1' Map.union (zipWith matchPattern ps vs)
matchPattern (PattTuple _ _) _ = undefined


-- Creates dummy values to give to the shader lambda expression, and then runs it.
interpretExprAsShader :: ShaderKind -> ValueEnv -> Expr -> Type -> InterpretM Value
interpretExprAsShader sk env e t =
  case t of
    TypeFun uniform_type (TypeFun texture_type (TypeFun varying_type result_type)) -> do
      -- Count the number of outputs.
      case (sk, result_type) of
        -- Vertex shader: (Real 4, a)
        (ShaderKindVertex, TypeTuple [TypeArray TypeReal (DimFix 4), output_type]) -> countOutputs sk output_type
        -- Fragment shader: Real 4
        (ShaderKindFragment, TypeArray TypeReal (DimFix 4)) -> return ()
        -- Fragment shader: (Bool, Real 4)
        (ShaderKindFragment, TypeTuple [TypeBool, TypeArray TypeReal (DimFix 4)]) -> return ()
        _ -> error $ "bad shader type <" ++ prettyType t ++ "> should have been caught by compiler!"
      
      -- Interpret the expression to create a closure.
      ValueFun f1 <- interpretExpr env e
      
      -- Count the number of uniforms, create a dummy variable to represent them, and apply closure.
      uniform_value <- dummyUniformValue sk uniform_type
      ValueFun f2 <- f1 uniform_value
      
      -- Count the number of textures, create a dummy variable to represent them, and apply closure.
      texture_value <- dummyTextureValue sk texture_type
      ValueFun f3 <- f2 texture_value
      
      -- Count the number of varyings, create a dummy variable to represent them, and apply closure.
      varying_value <- dummyVaryingValue sk varying_type
      f3 varying_value
      
    _ -> error $ "bad shader kind <" ++ prettyType t ++ "> should have been caught by compiler!"


-- Returns a dummy value and the number of uniforms.
dummyUniformValue :: ShaderKind -> Type -> InterpretM Value
dummyUniformValue _ (TypeUnit) =
  return ValueUnit
dummyUniformValue _ (TypeReal) = do
  i <- freshUniform
  n <- freshNode
  return $ ValueReal $ DFRealUniform n i
dummyUniformValue _ (TypeBool) = do
  i <- freshUniform
  n <- freshNode
  return $ ValueBool $ DFBoolUniform n i
dummyUniformValue sk (TypeArray t (DimFix d)) = do
  vs <- replicateM (fromIntegral d) (dummyUniformValue sk t)
  return $ ValueArray vs
dummyUniformValue sk (TypeTuple ts) = do
  vs <- mapM (dummyUniformValue sk) ts
  return $ ValueTuple vs
dummyUniformValue sk t = throwError $ ShaderError sk $ ShaderErrorBadUniformType t


-- Returns a dummy value and the number of textures.
dummyTextureValue :: ShaderKind -> Type -> InterpretM Value
dummyTextureValue _ (TypeUnit) =
  return ValueUnit
dummyTextureValue _ (TypeTex tk) = do
  i <- freshTexture tk
  n <- freshNode
  return $ ValueTex $ DFTexConstant n tk i
dummyTextureValue sk (TypeArray t (DimFix d)) = do
  vs <- replicateM (fromIntegral d) (dummyTextureValue sk t)
  return $ ValueArray vs
dummyTextureValue sk (TypeTuple ts) = do
  vs <- mapM (dummyTextureValue sk) ts
  return $ ValueTuple vs
dummyTextureValue sk t = throwError $ ShaderError sk $ ShaderErrorBadTextureType t


-- Returns a dummy value and the number of varyings.
dummyVaryingValue :: ShaderKind -> Type -> InterpretM Value
dummyVaryingValue _ (TypeUnit) =
  return ValueUnit
dummyVaryingValue _ (TypeReal) = do
  i <- freshVarying
  n <- freshNode
  return $ ValueReal $ DFRealVarying n i
dummyVaryingValue _ (TypeBool) = do
  i <- freshVarying
  n <- freshNode
  return $ ValueBool $ DFBoolVarying n i
dummyVaryingValue sk (TypeArray t (DimFix d)) = do
  vs <- replicateM (fromIntegral d) (dummyVaryingValue sk t)
  return $ ValueArray vs
dummyVaryingValue sk (TypeTuple ts) = do
  vs <- mapM (dummyVaryingValue sk) ts
  return $ ValueTuple vs
dummyVaryingValue sk t = throwError $ ShaderError sk $ ShaderErrorBadVaryingType t


-- Counts the number of outputs in the given output type.
countOutputs :: ShaderKind -> Type -> InterpretM ()
countOutputs _ (TypeUnit) = return ()
countOutputs _ (TypeReal) = freshGenericOutput
countOutputs _ (TypeBool) = freshGenericOutput
countOutputs sk (TypeArray t (DimFix d)) =
  replicateM_ (fromIntegral d) (countOutputs sk t)
countOutputs sk (TypeTuple ts) =
  mapM_ (countOutputs sk) ts
countOutputs sk t = throwError $ ShaderError sk $ ShaderErrorBadOutputType t
