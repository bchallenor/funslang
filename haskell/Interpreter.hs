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
interpretExprAsShader :: ValueEnv -> Expr -> Type -> Either String (Value, ShaderNumInputs)
interpretExprAsShader env e t =
  flip catchError (\s -> throwError $ s ++ "\nin expression with type: " ++ prettyType t) $ do
  case t of
    TypeFun uniform_type (TypeFun texture_type (TypeFun varying_type _)) -> do
      ValueFun f1 <- interpretExpr env e
      (uniform_value, nu) <- dummyUniformValue uniform_type
      ValueFun f2 <- f1 uniform_value
      (texture_value, nt) <- dummyTextureValue texture_type
      ValueFun f3 <- f2 texture_value
      (varying_value, nv) <- dummyVaryingValue varying_type
      v <- f3 varying_value
      return (v, ShaderNumInputs{num_uniforms = nu, num_textures = nt, num_varyings = nv})
    _ -> throwError "expression does not have correct kind to be a shader"


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
dummyUniformValue' (TypeArray t (DimFix n)) = do
  vs <- replicateM (fromIntegral n) (dummyUniformValue' t)
  return $ ValueArray vs
dummyUniformValue' (TypeTuple ts) = do
  vs <- mapM dummyUniformValue' ts
  return $ ValueTuple vs
dummyUniformValue' t = throwError $ "shader uniform arguments cannot have type <" ++ prettyType t ++ ">"


-- Returns a dummy value and the number of textures.
dummyTextureValue :: Type -> Either String (Value, Int)
dummyTextureValue t = do
  let (a, i') = runState (runErrorT $ dummyTextureValue' t) 0
  v <- a
  return (v, i')

dummyTextureValue' :: Type -> ErrorT String (State Int) Value
dummyTextureValue' (TypeUnit) =
  return ValueUnit
dummyTextureValue' (TypeTexture1D) = do
  i <- get
  put (i+1)
  return $ ValueTexture1D i
dummyTextureValue' (TypeTexture2D) = do
  i <- get
  put (i+1)
  return $ ValueTexture2D i
dummyTextureValue' (TypeTexture3D) = do
  i <- get
  put (i+1)
  return $ ValueTexture3D i
dummyTextureValue' (TypeTextureCube) = do
  i <- get
  put (i+1)
  return $ ValueTextureCube i
dummyTextureValue' (TypeArray t (DimFix n)) = do
  vs <- replicateM (fromIntegral n) (dummyTextureValue' t)
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
dummyVaryingValue' (TypeArray t (DimFix n)) = do
  vs <- replicateM (fromIntegral n) (dummyVaryingValue' t)
  return $ ValueArray vs
dummyVaryingValue' (TypeTuple ts) = do
  vs <- mapM dummyVaryingValue' ts
  return $ ValueTuple vs
dummyVaryingValue' t = throwError $ "shader varying arguments cannot have type <" ++ prettyType t ++ ">"
