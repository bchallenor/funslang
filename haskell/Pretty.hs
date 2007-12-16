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
prettyType = prettyDecodedType . decodeType

prettyDecodedType :: DecodedType -> String
prettyDecodedType (UnitDecodedType) = "()"
prettyDecodedType (RealDecodedType) = "Real"
prettyDecodedType (BoolDecodedType) = "Bool"
prettyDecodedType (Texture1DDecodedType) = "Texture1D"
prettyDecodedType (Texture2DDecodedType) = "Texture2D"
prettyDecodedType (Texture3DDecodedType) = "Texture3D"
prettyDecodedType (TextureCubeDecodedType) = "TextureCube"
prettyDecodedType (TupleDecodedType dts) = tuple $ map prettyDecodedType dts
prettyDecodedType (ArrayDecodedType dt i) = paren (isFunctionDecodedType dt) (prettyDecodedType dt) ++ " " ++ show i
prettyDecodedType (FunDecodedType dt1 dt2) = paren (isFunctionDecodedType dt1) (prettyDecodedType dt1) ++ " -> " ++ (prettyDecodedType dt2)
prettyDecodedType (TypeVarDecodedType tv) = tv
prettyDecodedType (DimVarDecodedType dt dv) = paren (isFunctionDecodedType dt) (prettyDecodedType dt) ++ " " ++ dv

-- We only need parens if the left subtype of a type is a function type.
isFunctionDecodedType :: DecodedType -> Bool
isFunctionDecodedType (FunDecodedType _ _) = True
isFunctionDecodedType _ = False

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
