module Typing where

import qualified Data.Map as Map
import qualified Data.List as List
import Control.Monad
import Representation
import Pretty


data TypeResult a
  = Typeable !a
  | Untypeable !String
  
  deriving (Show, Eq)

instance Monad TypeResult where
  (Typeable x) >>= k = k x
  (Untypeable s) >>= _ = Untypeable s
  return = Typeable
  fail = Untypeable

type Env = Map.Map String Type


inferType :: Expr -> TypeResult Type
inferType = inferType' Map.empty


inferType' :: Env -> Expr -> TypeResult Type

inferType' _ (UnitExpr) = return UnitType

inferType' _ (IntExpr _) = return IntType

inferType' _ (FloatExpr _) = return FloatType

inferType' _ (BoolExpr _) = return BoolType

inferType' gamma (VarExpr s) = do
  case Map.lookup s gamma of
    Just t -> return t
    Nothing -> fail ("unknown variable <" ++ s ++ ">")

inferType' _ (AppOp1Expr _ _) = do
  fail "todo" -- todo

inferType' _ (AppOp2Expr _ _ _) = do
  fail "todo" -- todo

inferType' gamma (AppExpr f x) = do
  tf <- inferType' gamma f
  tx <- inferType' gamma x
  case tf of
    FunType ta tb -> do
      if ta == tx
        then return tb
        else fail ("<" ++ prettyExpr x ++ "> has type " ++ prettyType tx ++ " but function expects type " ++ prettyType ta)
    _ -> fail ("<" ++ prettyExpr f ++ "> is not of function type")

inferType' gamma (ArrayExpr es) = do
  ts <- mapM (inferType' gamma) es
  case List.nub ts of
    [t] -> return (ArrayType t (toInteger (length es)))
    _ -> fail ("elements in array <" ++ prettyExpr (ArrayExpr es) ++ "> do not all have same type")

inferType' gamma (TupleExpr es) = do
  ts <- mapM (inferType' gamma) es
  return (TupleType ts)

inferType' gamma (IfExpr ec et ef) = do
  tc <- inferType' gamma ec
  case tc of
    BoolType -> do
      tt <- inferType' gamma et
      tf <- inferType' gamma ef
      if tt == tf
        then return tt
        else fail ("arms of <" ++ prettyExpr (IfExpr ec et ef) ++ "> must have same type")
      return UnitType
    _ -> fail ("condition of <" ++ prettyExpr (IfExpr ec et ef) ++ "> must have " ++ prettyType BoolType ++ " type")

-- inferType' gamma (LetExpr p ea eb) =
-- inferType' gamma (LambdaExpr p t e) =



-- matchPatternWithTypedExpr :: Patt -> Expr -> Maybe [(String,Expr)]
-- matchPatternWithExpr (UnitPatt) e =
--   case of 
-- matchPatternWithExpr (VarPatt s) e =
-- matchPatternWithExpr (ArrayPatt ps) e =
-- matchPatternWithExpr (TuplePatt ps) e =