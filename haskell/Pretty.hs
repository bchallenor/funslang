module Pretty(prettyType, prettyExpr, prettyPatt) where

import Data.List as List
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

prettyType :: Type -> String
prettyType (UnitType) = "()"
prettyType (NumType) = "Num"
prettyType (BoolType) = "Bool"
prettyType (Texture1DType) = "Texture1D"
prettyType (Texture2DType) = "Texture2D"
prettyType (Texture3DType) = "Texture3D"
prettyType (TextureCubeType) = "TextureCube"
prettyType (TupleType ts) = tuple (map prettyType ts)
prettyType (ArrayType t i) =
  let s b = paren b (prettyType t) ++ " " ++ show i in
  case t of
    ArrayType _ _ -> s False
    _ -> s $ not $ atomType t
prettyType (FunType t1 t2) =
  let s b = paren b (prettyType t1) ++ " -> " ++ prettyType t2 in
    s $ not $ atomType t1

atomType :: Type -> Bool
atomType (UnitType) = True
atomType (NumType) = True
atomType (BoolType) = True
atomType (Texture1DType) = True
atomType (Texture2DType) = True
atomType (Texture3DType) = True
atomType (TextureCubeType) = True
atomType (TupleType ts) = True
atomType (ArrayType t i) = False
atomType (FunType t1 t2) = False

prettyPatt :: Patt -> String
prettyPatt (WildPatt) = "_"
prettyPatt (UnitPatt) = "()"
prettyPatt (VarPatt s) = s
prettyPatt (ArrayPatt ps) = array (map prettyPatt ps)
prettyPatt (TuplePatt ps) = tuple (map prettyPatt ps)

prettyExpr :: Expr -> String
prettyExpr (UnitExpr) = "()"
prettyExpr (NumExpr d) = show d
prettyExpr (BoolExpr b) = show b
prettyExpr (VarExpr s) = s
prettyExpr (AppExpr e1 e2) = paren True $ paren True (prettyExpr e1) ++ " " ++ prettyExpr e2
prettyExpr (ArrayExpr es) = array (map prettyExpr es)
prettyExpr (TupleExpr es) = tuple (map prettyExpr es)
prettyExpr (IfExpr ec et ef) = "if " ++ prettyExpr ec ++ " then " ++ prettyExpr et ++ " else " ++ prettyExpr ef
prettyExpr (LetExpr p ea eb) = "let " ++ prettyPatt p ++ " = " ++ prettyExpr ea ++ " in " ++ prettyExpr eb
prettyExpr (LambdaExpr p e) = "\\ " ++ prettyPatt p ++ " . " ++ prettyExpr e
