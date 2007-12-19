module Interpreter where

import qualified Data.Map as Map
import Control.Monad.Error

import Representation


-- The environment of values as being interpreted.
type ValueEnv = Map.Map String Value


-- Takes a boolean condition and zips up the two values with DFCond nodes,
-- so that the resulting value is either the first or second value according to the condition.
conditionalize :: DF -> Value -> Value -> Either String Value
conditionalize dfb (ValueDF df1) (ValueDF df2) = return $ ValueDF $ DFCond (dfType df1) dfb df1 df2
conditionalize dfb (ValueArray vs1) (ValueArray vs2) = do
  vs <- zipWithM (conditionalize dfb) vs1 vs2
  return $ ValueArray vs
conditionalize dfb (ValueTuple vs1) (ValueTuple vs2) = do
  vs <- zipWithM (conditionalize dfb) vs1 vs2
  return $ ValueTuple vs
conditionalize _ _ _ = throwError $ "values in if expression cannot be compared at run time"


-- The inner interpreter function.
-- Returns the value, or on error, a message explaining the error and the expression that triggered it.
interpret' :: ValueEnv -> Expr -> Either String Value

interpret' env (ExprUnitLiteral) = return ValueUnit

interpret' env (ExprRealLiteral b) = return $ ValueDF $ DFRealLiteral b

interpret' env (ExprBoolLiteral b) = return $ ValueDF $ DFBoolLiteral b

interpret' env (ExprVar ident) =
  case Map.lookup ident env of
    Just v -> return v
    Nothing -> throwError $ "variable <" ++ ident ++ "> undefined"

interpret' env (ExprApp e1 e2) = do
  ValueFun f <- interpret' env e1
  v <- interpret' env e2
  f v

interpret' env (ExprArray es) = do
  vs <- mapM (interpret' env) es
  return $ ValueArray vs

interpret' env (ExprTuple es) = do
  vs <- mapM (interpret' env) es
  return $ ValueTuple vs

interpret' env (ExprIf eb e1 e2) = do
  ValueDF dfb <- interpret' env eb
  v1 <- interpret' env e1
  v2 <- interpret' env e2
  if v1 == v2
    then return v1 -- optimize out if both branches are the same
    else case dfb of
      DFBoolLiteral b -> return $ if b then v1 else v2 -- optimize out if condition statically known
      _ -> conditionalize dfb v1 v2 -- runtime condition and runtime values




-- interpret' env (ExprLet p e1 e2)
-- interpret' env (ExprLambda p e)





-- a = ValueFun (\(ValueReal dfr1) -> ValueFun (\(ValueReal dfr2) -> ValueReal (DFRealAdd dfr1 dfr2)))
-- app (ValueFun f) v = f v

-- b = app (app a (ValueReal (DFRealLiteral 1))) (ValueReal (DFRealLiteral 2))