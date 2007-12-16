module Pretty(prettyType, prettyExpr, prettyPatt) where

import Control.Monad.State
import Data.List as List
import qualified Data.Map as Map
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


prettyType :: Type -> String
prettyType t = evalState (prettyType' t) (('a', Map.empty), ('n', Map.empty))

prettyType' :: Type -> State VarRefDecodeContext String
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
prettyType' (ArrayType t i) = do
  st <- prettyType' t
  return $ paren (isFunctionType t) st ++ " " ++ show i
prettyType' (FunType t1 t2) = do
  st1 <- prettyType' t1
  st2 <- prettyType' t2
  return $ paren (isFunctionType t1) st1 ++ " -> " ++ st2
prettyType' (TypeVarType ref) = do
  ((tvnext, tvmap), (dvnext, dvmap)) <- get
  case Map.lookup ref tvmap of
    Just c -> return ("'" ++ [c])
    Nothing -> do
      put ((succ tvnext, Map.insert ref tvnext tvmap), (dvnext, dvmap))
      prettyType' (TypeVarType ref)
prettyType' (DimVarType t ref) = do
  ((tvnext, tvmap), (dvnext, dvmap)) <- get
  case Map.lookup ref dvmap of
    Just c -> do
      st <- prettyType' t
      return (st ++ " " ++ [c])
    Nothing -> do
      put ((tvnext, tvmap), (succ dvnext, Map.insert ref dvnext dvmap))
      prettyType' (DimVarType t ref)

-- We only need parens if the left subtype of a type is a function type.
isFunctionType :: Type -> Bool
isFunctionType (FunType _ _) = True
isFunctionType _ = False

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
