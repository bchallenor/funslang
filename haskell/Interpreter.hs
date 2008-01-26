module Interpreter(interpretExpr, interpretExprAsShader, ValueEnv) where

import qualified Data.Map as Map
import qualified Data.List as List
import Control.Monad.Error
import Control.Monad.State

import Representation
import Pretty


-- The environment of values as being interpreted.
type ValueEnv = Map.Map String Value


-- The inner interpreter function.
-- Returns the value, or on error, a message explaining the error and the expression that triggered it.
interpretExpr :: ValueEnv -> Expr -> Either String Value

interpretExpr _ (ExprUnitLiteral) = return ValueUnit

interpretExpr _ (ExprRealLiteral b) = return $ ValueDFReal $ DFRealLiteral b

interpretExpr _ (ExprBoolLiteral b) = return $ ValueDFBool $ DFBoolLiteral b

interpretExpr env a@(ExprVar ident) =
  flip catchError (\s -> throwError $ s ++ "\nin expression: " ++ prettyExpr a) $ do
  case Map.lookup ident env of
    Just v -> return v
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
      DFBoolLiteral b -> return $ if b then v1 else v2 -- optimize out if condition statically known
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
conditionalize :: DFBool -> Value -> Value -> Either String Value
conditionalize _ (ValueUnit) (ValueUnit) = return ValueUnit
conditionalize dfb (ValueDFReal df1) (ValueDFReal df2) = return $ ValueDFReal $ DFRealCond dfb df1 df2
conditionalize dfb (ValueDFBool df1) (ValueDFBool df2) = return $ ValueDFBool $ DFBoolCond dfb df1 df2
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
matchPattern (PattVar ident _) v = Map.singleton ident v
matchPattern (PattArray ps _) (ValueArray vs) = List.foldl1' Map.union (zipWith matchPattern ps vs)
matchPattern (PattArray _ _) _ = undefined
matchPattern (PattTuple ps _) (ValueTuple vs) = List.foldl1' Map.union (zipWith matchPattern ps vs)
matchPattern (PattTuple _ _) _ = undefined


-- Creates dummy values to give to the shader lambda expression, and then runs it.
interpretExprAsShader :: ShaderKind -> ValueEnv -> Expr -> Type -> Either String (Value, ShaderInputOutput)
interpretExprAsShader sk env e t =
  flip catchError (\s -> throwError $ s ++ "\nin expression with type: " ++ prettyType t) $ do
  case t of
    TypeFun uniform_type (TypeFun texture_type (TypeFun varying_type result_type)) -> do
      -- Count the number of outputs.
      no <- case (sk, result_type) of
        (ShaderKindVertex, TypeTuple [TypeArray TypeReal (DimFix 4), output_type]) -> countOutputs output_type
        (ShaderKindVertex, _) -> throwError "vertex shader has correct kind, but incorrect return type"
        (ShaderKindFragment, TypeArray TypeReal (DimFix 4)) -> return 0
        (ShaderKindFragment, _) -> throwError "fragment shader has correct kind, but incorrect return type"
      
      -- Interpret the expression to create a closure.
      ValueFun f1 <- interpretExpr env e
      
      -- Count the number of uniforms, create a dummy variable to represent them, and apply closure.
      (uniform_value, nu) <- dummyUniformValue uniform_type
      ValueFun f2 <- f1 uniform_value
      
      -- Count the number of textures, create a dummy variable to represent them, and apply closure.
      (texture_value, nt, ts) <- dummyTextureValue texture_type
      ValueFun f3 <- f2 texture_value
      
      -- Count the number of varyings, create a dummy variable to represent them, and apply closure.
      (varying_value, nv) <- dummyVaryingValue varying_type
      v <- f3 varying_value
      
      return (v, ShaderInputOutput{num_uniforms = nu, num_textures = nt, num_varyings = nv, textures = ts, num_outputs = no})
      
    _ -> throwError "shader does not have correct kind"


-- Returns a dummy value and the number of uniforms.
dummyUniformValue :: Type -> Either String (Value, Int)
dummyUniformValue t = do
  let (a, i') = runState (runErrorT $ dummyUniformValue' t) 0
  v <- a
  return (v, i')

dummyUniformValue' :: Type -> ErrorT String (State Int) Value
dummyUniformValue' (TypeUnit) =
  return ValueUnit
dummyUniformValue' (TypeReal) = do
  i <- get
  put (i+1)
  return $ ValueDFReal $ DFRealUniform i
dummyUniformValue' (TypeBool) = do
  i <- get
  put (i+1)
  return $ ValueDFBool $ DFBoolUniform i
dummyUniformValue' (TypeArray t (DimFix d)) = do
  vs <- replicateM (fromIntegral d) (dummyUniformValue' t)
  return $ ValueArray vs
dummyUniformValue' (TypeTuple ts) = do
  vs <- mapM dummyUniformValue' ts
  return $ ValueTuple vs
dummyUniformValue' t = throwError $ "shader uniform arguments cannot have type <" ++ prettyType t ++ ">"


-- Returns a dummy value and the number of textures.
dummyTextureValue :: Type -> Either String (Value, Int, [ShaderTextureInput])
dummyTextureValue t = do
  let (a, (i, ts)) = runState (runErrorT $ dummyTextureValue' t) (0, [])
  v <- a
  return (v, i, ts)

dummyTextureValue' :: Type -> ErrorT String (State (Int, [ShaderTextureInput])) Value
dummyTextureValue' (TypeUnit) =
  return ValueUnit
dummyTextureValue' (TypeTexture1D) = do
  (i, ts) <- get
  put (i+1, (ShaderTextureInput1D i) : ts)
  return $ ValueTexture1D i
dummyTextureValue' (TypeTexture2D) = do
  (i, ts) <- get
  put (i+1, (ShaderTextureInput2D i) : ts)
  return $ ValueTexture2D i
dummyTextureValue' (TypeTexture3D) = do
  (i, ts) <- get
  put (i+1, (ShaderTextureInput3D i) : ts)
  return $ ValueTexture3D i
dummyTextureValue' (TypeTextureCube) = do
  (i, ts) <- get
  put (i+1, (ShaderTextureInputCube i) : ts)
  return $ ValueTextureCube i
dummyTextureValue' (TypeArray t (DimFix d)) = do
  vs <- replicateM (fromIntegral d) (dummyTextureValue' t)
  return $ ValueArray vs
dummyTextureValue' (TypeTuple ts) = do
  vs <- mapM dummyTextureValue' ts
  return $ ValueTuple vs
dummyTextureValue' t = throwError $ "shader texture arguments cannot have type <" ++ prettyType t ++ ">"


-- Returns a dummy value and the number of varyings.
dummyVaryingValue :: Type -> Either String (Value, Int)
dummyVaryingValue t = do
  let (a, i') = runState (runErrorT $ dummyVaryingValue' t) 0
  v <- a
  return (v, i')

dummyVaryingValue' :: Type -> ErrorT String (State Int) Value
dummyVaryingValue' (TypeUnit) =
  return ValueUnit
dummyVaryingValue' (TypeReal) = do
  i <- get
  put (i+1)
  return $ ValueDFReal $ DFRealVarying i
dummyVaryingValue' (TypeBool) = do
  i <- get
  put (i+1)
  return $ ValueDFBool $ DFBoolVarying i
dummyVaryingValue' (TypeArray t (DimFix d)) = do
  vs <- replicateM (fromIntegral d) (dummyVaryingValue' t)
  return $ ValueArray vs
dummyVaryingValue' (TypeTuple ts) = do
  vs <- mapM dummyVaryingValue' ts
  return $ ValueTuple vs
dummyVaryingValue' t = throwError $ "shader varying arguments cannot have type <" ++ prettyType t ++ ">"


-- Counts the number of outputs in the given output type.
countOutputs :: Type -> Either String Int
countOutputs (TypeUnit) = return 0
countOutputs (TypeReal) = return 1
countOutputs (TypeBool) = return 1
countOutputs (TypeArray t (DimFix d)) = do
  n <- countOutputs t
  return $ fromIntegral d * n
countOutputs (TypeTuple ts) = do
  ns <- mapM (countOutputs) ts
  return $ sum ns
countOutputs t = throwError $ "shader output cannot have type <" ++ prettyType t ++ ">"
