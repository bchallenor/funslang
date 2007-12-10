module Typing where

import qualified Data.Map as Map
import qualified Data.List as List
import Representation
import Pretty


data TypeResult a
  = Typeable !a
  | Untypeable !String
  
  deriving (Show, Eq)

instance Monad TypeResult where
  (Typeable x) >>= k = k x
  (Untypeable s) >>= k = Untypeable s
  return = Typeable
  fail = Untypeable

type Env = Map.Map String Type


inferType :: Env -> Expr -> TypeResult Type
inferType _ (UnitExpr) = return UnitType
inferType _ (IntExpr i) = return IntType
inferType _ (FloatExpr d) = return FloatType
inferType _ (BoolExpr b) = return BoolType
inferType gamma (VarExpr s) = do
  case Map.lookup s gamma of
    Just t -> return t
    Nothing -> fail ("unknown variable <" ++ s ++ ">")
inferType _ (AppOp1Expr op e1) = do
  fail "todo" -- todo
inferType _ (AppOp2Expr op e1 e2) = do
  fail "todo" -- todo
inferType gamma (AppExpr f x) = do
  tf <- inferType gamma f
  tx <- inferType gamma x
  let FunType ta tb = tf
  if ta == tx then return tb else fail ("<" ++ prettyExpr x ++ "> has type " ++ prettyType tx ++ " but function expects type " ++ prettyType ta)
inferType gamma (ArrayExpr es) = do
  case List.nub (map (inferType gamma) es) of
    [Typeable t] -> return (ArrayType t (toInteger (length es)))
    _ -> fail ("elements in array <" ++ prettyExpr (ArrayExpr es) ++ "> do not all have same type")

-- inferType (TupleExpr es) =
-- inferType (IfExpr ec et ef) =
-- inferType (LetExpr p ea eb) =
-- inferType (LambdaExpr p t e) =