module Interpreter(interpretExpr, interpretExprAsShader, ValueEnv) where

import qualified Data.Map as Map
import qualified Data.List as List
import Control.Monad.Error
import Control.Monad.State

import Representation
import Pretty


-- The environment of values as being interpreted.
type ValueEnv = Map.Map String (InterpretM Value)


-- The inner interpreter function.
-- Returns the value, or on error, a message explaining the error and the expression that triggered it.
interpretExpr :: ValueEnv -> Expr -> InterpretM Value

interpretExpr _ (ExprUnitLiteral) = return ValueUnit

interpretExpr _ (ExprRealLiteral b) = do
  n <- freshNode
  return $ ValueDFReal $ DFRealLiteral n b

interpretExpr _ (ExprBoolLiteral b) =  do
  n <- freshNode
  return $ ValueDFBool $ DFBoolLiteral n b

interpretExpr env a@(ExprVar ident) =
  flip catchError (\s -> throwError $ s ++ "\nin expression: " ++ prettyExpr a) $ do
  case Map.lookup ident env of
    Just v -> v
    Nothing -> throwError $ "variable <" ++ ident ++ "> undefined"

interpretExpr env a@(ExprApp e1 e2) =
  flip catchError (\s -> throwError $ s ++ "\nin expression: " ++ prettyExpr a) $ do
  ValueFun f <- interpretExpr env e1
  v <- interpretExpr env e2
  f v

interpretExpr env a@(ExprArray es) =
  flip catchError (\s -> throwError $ s ++ "\nin expression: " ++ prettyExpr a) $ do
  vs <- mapM (interpretExpr env) es
  return $ ValueArray vs

interpretExpr env a@(ExprTuple es) =
  flip catchError (\s -> throwError $ s ++ "\nin expression: " ++ prettyExpr a) $ do
  vs <- mapM (interpretExpr env) es
  return $ ValueTuple vs

interpretExpr env a@(ExprIf eb e1 e2) =
  flip catchError (\s -> throwError $ s ++ "\nin expression: " ++ prettyExpr a) $ do
  ValueDFBool dfb <- interpretExpr env eb
  v1 <- interpretExpr env e1
  v2 <- interpretExpr env e2
  if v1 == v2
    then return v1 -- optimize out if both branches are the same
    else case dfb of
      DFBoolLiteral _ b -> return $ if b then v1 else v2 -- optimize out if condition statically known
      _ -> conditionalize dfb v1 v2 -- runtime condition and runtime values

interpretExpr env a@(ExprLet p e1 e2) =
  flip catchError (\s -> throwError $ s ++ "\nin expression: " ++ prettyExpr a) $ do
  v1 <- interpretExpr env e1
  let env' = env `Map.union` matchPattern p v1
  v2 <- interpretExpr env' e2
  return v2

interpretExpr env a@(ExprLambda p e) =
  flip catchError (\s -> throwError $ s ++ "\nin expression: " ++ prettyExpr a) $ do
  return $ ValueFun (\v -> let env' = env `Map.union` matchPattern p v in interpretExpr env' e)


-- Takes a boolean condition and zips up the two values with DFCond nodes,
-- so that the resulting value is either the first or second value according to the condition.
conditionalize :: DFBool -> Value -> Value -> InterpretM Value
conditionalize _ (ValueUnit) (ValueUnit) = return ValueUnit
conditionalize dfb (ValueDFReal df1) (ValueDFReal df2) =  do
  n <- freshNode
  return $ ValueDFReal $ DFRealCond n dfb df1 df2
conditionalize dfb (ValueDFBool df1) (ValueDFBool df2) =  do
  n <- freshNode
  return $ ValueDFBool $ DFBoolCond n dfb df1 df2
conditionalize dfb (ValueArray vs1) (ValueArray vs2) = do
  vs <- zipWithM (conditionalize dfb) vs1 vs2
  return $ ValueArray vs
conditionalize dfb (ValueTuple vs1) (ValueTuple vs2) = do
  vs <- zipWithM (conditionalize dfb) vs1 vs2
  return $ ValueTuple vs
conditionalize _ _ _ = throwError $ "if expression would violate run time model"


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
interpretExprAsShader :: ShaderKind -> ValueEnv -> Expr -> Type -> ShaderState -> Either String (Value, ShaderState)
interpretExprAsShader sk env e t si = do
  (v, s) <- runInterpretM (interpretExprAsShader' sk env e t) si
  return (v, s)

interpretExprAsShader' :: ShaderKind -> ValueEnv -> Expr -> Type -> InterpretM Value
interpretExprAsShader' sk env e t =
  flip catchError (\s -> throwError $ s ++ "\nin expression with type: " ++ prettyType t) $ do
  case t of
    TypeFun uniform_type (TypeFun texture_type (TypeFun varying_type result_type)) -> do
      -- Count the number of outputs.
      case (sk, result_type) of
        -- Vertex shader: (Real 4, 'a)
        (ShaderKindVertex, TypeTuple [TypeArray TypeReal (DimFix 4), output_type]) -> countOutputs output_type
        -- Fragment shader: Real 4
        (ShaderKindFragment, TypeArray TypeReal (DimFix 4)) -> return ()
        -- Fragment shader: (Bool, Real 4)
        (ShaderKindFragment, TypeTuple [TypeBool, TypeArray TypeReal (DimFix 4)]) -> return ()
        _ -> error "unknown shader type passed to interpreter"
      
      -- Interpret the expression to create a closure.
      ValueFun f1 <- interpretExpr env e
      
      -- Count the number of uniforms, create a dummy variable to represent them, and apply closure.
      uniform_value <- dummyUniformValue uniform_type
      ValueFun f2 <- f1 uniform_value
      
      -- Count the number of textures, create a dummy variable to represent them, and apply closure.
      texture_value <- dummyTextureValue texture_type
      ValueFun f3 <- f2 texture_value
      
      -- Count the number of varyings, create a dummy variable to represent them, and apply closure.
      varying_value <- dummyVaryingValue varying_type
      f3 varying_value
      
    _ -> throwError "shader does not have correct kind"


-- Returns a dummy value and the number of uniforms.
dummyUniformValue :: Type -> InterpretM Value
dummyUniformValue (TypeUnit) =
  return ValueUnit
dummyUniformValue (TypeReal) = do
  i <- freshUniform
  n <- freshNode
  return $ ValueDFReal $ DFRealUniform n i
dummyUniformValue (TypeBool) = do
  i <- freshUniform
  n <- freshNode
  return $ ValueDFBool $ DFBoolUniform n i
dummyUniformValue (TypeArray t (DimFix d)) = do
  vs <- replicateM (fromIntegral d) (dummyUniformValue t)
  return $ ValueArray vs
dummyUniformValue (TypeTuple ts) = do
  vs <- mapM dummyUniformValue ts
  return $ ValueTuple vs
dummyUniformValue t = throwError $ "shader uniform arguments cannot have type <" ++ prettyType t ++ ">"


-- Returns a dummy value and the number of textures.
dummyTextureValue :: Type -> InterpretM Value
dummyTextureValue (TypeUnit) =
  return ValueUnit
dummyTextureValue (TypeTexture1D) = do
  i <- freshTexture ShaderTextureInput1D
  return $ ValueTexture1D i
dummyTextureValue (TypeTexture2D) = do
  i <- freshTexture ShaderTextureInput2D
  return $ ValueTexture2D i
dummyTextureValue (TypeTexture3D) = do
  i <- freshTexture ShaderTextureInput3D
  return $ ValueTexture3D i
dummyTextureValue (TypeTextureCube) = do
  i <- freshTexture ShaderTextureInputCube
  return $ ValueTextureCube i
dummyTextureValue (TypeArray t (DimFix d)) = do
  vs <- replicateM (fromIntegral d) (dummyTextureValue t)
  return $ ValueArray vs
dummyTextureValue (TypeTuple ts) = do
  vs <- mapM dummyTextureValue ts
  return $ ValueTuple vs
dummyTextureValue t = throwError $ "shader texture arguments cannot have type <" ++ prettyType t ++ ">"


-- Returns a dummy value and the number of varyings.
dummyVaryingValue :: Type -> InterpretM Value
dummyVaryingValue (TypeUnit) =
  return ValueUnit
dummyVaryingValue (TypeReal) = do
  i <- freshVarying
  n <- freshNode
  return $ ValueDFReal $ DFRealVarying n i
dummyVaryingValue (TypeBool) = do
  i <- freshVarying
  n <- freshNode
  return $ ValueDFBool $ DFBoolVarying n i
dummyVaryingValue (TypeArray t (DimFix d)) = do
  vs <- replicateM (fromIntegral d) (dummyVaryingValue t)
  return $ ValueArray vs
dummyVaryingValue (TypeTuple ts) = do
  vs <- mapM dummyVaryingValue ts
  return $ ValueTuple vs
dummyVaryingValue t = throwError $ "shader varying arguments cannot have type <" ++ prettyType t ++ ">"


-- Counts the number of outputs in the given output type.
countOutputs :: Type -> InterpretM ()
countOutputs (TypeUnit) = return ()
countOutputs (TypeReal) = freshGenericOutput
countOutputs (TypeBool) = freshGenericOutput
countOutputs (TypeArray t (DimFix d)) =
  replicateM_ (fromIntegral d) (countOutputs t)
countOutputs (TypeTuple ts) =
  mapM_ countOutputs ts
countOutputs t = throwError $ "shader output cannot have type <" ++ prettyType t ++ ">"
