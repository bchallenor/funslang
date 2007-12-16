module Pretty(prettyType, prettyExpr, prettyPatt) where

import Control.Monad.State
import Data.List as List
import qualified Data.IntMap as IntMap
import Representation

paren :: Bool -> [Char] -> [Char]
paren True s = "(" ++ s ++ ")"
paren False s = s

brack :: Bool -> [Char] -> [Char]
brack True s = "[" ++ s ++ "]"
brack False s = s

comma :: [[Char]] -> [Char]
comma = concat . List.intersperse ", "

space :: [[Char]] -> [Char]
space = concat . List.intersperse " "

tuple :: [[Char]] -> [Char]
tuple = paren True . comma

array :: [[Char]] -> [Char]
array = brack True . comma


-- To print a type we look from left to right for TypeVarRefs, replacing them with
-- 'a, 'b, etc, and similarly for DimVarRefs, replacing them with m, n, etc.
type VarRefContext = (Char, IntMap.IntMap Char) -- next available var, existing var ref to var mappings
type VarRefContexts = (VarRefContext, VarRefContext) -- type vars, dim vars

prettyType :: Type -> String
prettyType t = evalState (prettyType' t) (('a', IntMap.empty), ('n', IntMap.empty))

prettyType' :: Type -> State VarRefContexts String
prettyType' (UnitType) = return "()"
prettyType' (RealType) = return "Real"
prettyType' (BoolType) = return "Bool"
prettyType' (Texture1DType) = return "Texture1D"
prettyType' (Texture2DType) = return "Texture2D"
prettyType' (Texture3DType) = return "Texture3D"
prettyType' (TextureCubeType) = return "TextureCube"
prettyType' (TupleType ts) = do
  sts <- mapM prettyType' ts
  return $ tuple sts
prettyType' (ArrayType t i) =
  let s b = do {
    st <- prettyType' t;
    return (paren b st ++ " " ++ show i) } in
  case t of
    ArrayType _ _ -> s False
    _ -> s $ not $ atomType t
prettyType' (FunType t1 t2) =
  let s b = do {
    st1 <- prettyType' t1;
    st2 <- prettyType' t2;
    return (paren b st1 ++ " -> " ++ st2) } in
  s $ not $ atomType t1
prettyType' (TypeVarType ref) = do
  ((tvnext, tvmap), (dvnext, dvmap)) <- get
  case IntMap.lookup ref tvmap of
    Just c -> return ('\'':c:"")
    Nothing -> do
      put ((succ tvnext, IntMap.insert ref tvnext tvmap), (dvnext, dvmap))
      prettyType' (TypeVarType ref)
prettyType' (DimVarType t ref) = do
  ((tvnext, tvmap), (dvnext, dvmap)) <- get
  case IntMap.lookup ref dvmap of
    Just c -> do
      st <- prettyType' t
      return (st ++ " " ++ [c])
    Nothing -> do
      put ((tvnext, tvmap), (succ dvnext, IntMap.insert ref dvnext dvmap))
      prettyType' (DimVarType t ref)

atomType :: Type -> Bool
atomType (UnitType) = True
atomType (RealType) = True
atomType (BoolType) = True
atomType (Texture1DType) = True
atomType (Texture2DType) = True
atomType (Texture3DType) = True
atomType (TextureCubeType) = True
atomType (TupleType ts) = True
atomType (ArrayType t i) = False
atomType (FunType t1 t2) = False
atomType (TypeVarType ref) = True
atomType (DimVarType t ref) = False

prettyPatt :: Patt -> String
prettyPatt (WildPatt) = "_"
prettyPatt (UnitPatt) = "()"
prettyPatt (VarPatt s) = s
prettyPatt (ArrayPatt ps) = array (map prettyPatt ps)
prettyPatt (TuplePatt ps) = tuple (map prettyPatt ps)

prettyExpr :: Expr -> String
prettyExpr (UnitConstExpr) = "()"
prettyExpr (RealConstExpr d) = show d
prettyExpr (BoolConstExpr b) = show b
prettyExpr (VarExpr s) = s
prettyExpr (AppExpr e1 e2) = paren True $ paren True (prettyExpr e1) ++ " " ++ prettyExpr e2
prettyExpr (ArrayExpr es) = array (map prettyExpr es)
prettyExpr (TupleExpr es) = tuple (map prettyExpr es)
prettyExpr (IfExpr ec et ef) = "if " ++ prettyExpr ec ++ " then " ++ prettyExpr et ++ " else " ++ prettyExpr ef
prettyExpr (LetExpr p ea eb) = "let " ++ prettyPatt p ++ " = " ++ prettyExpr ea ++ " in " ++ prettyExpr eb
prettyExpr (LambdaExpr p e) = "\\ " ++ prettyPatt p ++ " . " ++ prettyExpr e
