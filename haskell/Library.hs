module Library(libraryTypeSchemes, libraryValues, queryTypeScheme) where

import qualified Data.ByteString.Lazy.Char8 as ByteString
import qualified Data.List as List
import qualified Data.Map as Map
import Control.Monad.Error

import Typing
import Parser
import Representation
import Interpreter


data Fixity
  = Prefix
  | InfixL
  | InfixR
  | InfixN
  | Postfix
  
  deriving (Show, Eq)


-- A tuple of:
-- - a map from library identifiers to type schemes;
-- - the subsequent list of fresh variable references, given that some were used
--   in constructing the type schemes of the library functions.
libraryTypeSchemes :: (SchemeEnv, ([TypeVarRef], [DimVarRef]))
libraryTypeSchemes =
  List.foldl' (
    \ (gamma, vrefs) (_, ident, tstr, _, _, _, _) ->
      case parseType vrefs (ByteString.pack tstr) of
        Right (t, vrefs') ->
          let sigma = Scheme (fvType t) t in
            (Map.insert ident sigma gamma, vrefs')
        Left msg -> error $ "library function type does not parse: " ++ msg
  ) (Map.empty, initFreshVarRefs) library


-- The values that the library provides.
libraryValues :: ValueEnv
libraryValues =
  List.foldl' (
      \ env (_, ident, _, _, _, _, v) ->
        Map.insert ident v env
    ) Map.empty library


-- Queries the library, for debugging purposes.
queryTypeScheme :: String -> String
queryTypeScheme ident =
  let (m, _) = libraryTypeSchemes in
    case Map.lookup ident m of
      Just sigma -> show sigma
      Nothing -> "not in library"


-- Creates a function value which applies the given unary operator.
makeValueFun1 :: (Value -> dfa) -> (dfa -> dfb) -> (dfb -> Value) -> Value
makeValueFun1 unbox op box =
  ValueFun $ \ v1 -> Right $
    box $ op (unbox v1)

-- Creates a function value which applies the given binary operator.
makeValueFun2 :: (Value -> dfa) -> (dfa -> dfa -> dfb) -> (dfb -> Value) -> Value
makeValueFun2 unbox op box =
  ValueFun $ \ v1 -> Right $
    ValueFun $ \ v2 -> Right $
      box $ op (unbox v1) (unbox v2)

-- Creates a function value which applies the given trinary operator.
makeValueFun3 :: (Value -> dfa) -> (dfa -> dfa -> dfa -> dfb) -> (dfb -> Value) -> Value
makeValueFun3 unbox op box =
  ValueFun $ \ v1 -> Right $
    ValueFun $ \ v2 -> Right $
      ValueFun $ \ v3 -> Right $
      box $ op (unbox v1) (unbox v2) (unbox v3)


-- Higher order functions.

valueFun_map :: Value
valueFun_map =
  ValueFun $ \ (ValueFun f) -> Right $
    ValueFun $ \ (ValueArray as) -> do
      vs <- mapM f as
      return $ ValueArray vs

valueFun_foldl :: Value
valueFun_foldl =
  ValueFun $ \ (ValueFun f) -> Right $
    ValueFun $ \ va -> Right $
      ValueFun $ \ (ValueArray vbs) ->
        valueFun_foldl' f va vbs

valueFun_foldl' :: (Value -> Either String Value) -> Value -> [Value] -> Either String Value
valueFun_foldl' _ z [] = return z
valueFun_foldl' f z (x:xs) = do
  ValueFun fz <- f z
  fzx <- fz x
  valueFun_foldl' f fzx xs

valueFun_foldr :: Value
valueFun_foldr =
  ValueFun $ \ (ValueFun f) -> Right $
    ValueFun $ \ vb -> Right $
      ValueFun $ \ (ValueArray vas) ->
        valueFun_foldr' f vb vas

valueFun_foldr' :: (Value -> Either String Value) -> Value -> [Value] -> Either String Value
valueFun_foldr' _ z [] = return z
valueFun_foldr' f z (x:xs) = do
  z' <- valueFun_foldr' f z xs
  ValueFun fx <- f x
  fx z'

valueFun_unroll :: Value
valueFun_unroll =
  ValueFun $ \ (ValueFun f) -> Right $
    ValueFun $ \ (ValueDFReal dfn) -> Right $
      ValueFun $ \ z ->
        case dfn of
          DFRealLiteral n -> valueFun_unroll' f (floor n) z
          _ -> throwError $ "in function 'unroll', n must be statically determinable"

valueFun_unroll' :: (Value -> Either String Value) -> Int -> Value -> Either String Value
valueFun_unroll' _ 0 z = return z
valueFun_unroll' f n z = do
  fz <- f z
  valueFun_unroll' f (n-1) fz

valueFun_zipWith :: Value
valueFun_zipWith =
  ValueFun $ \ (ValueFun f) -> Right $
    ValueFun $ \ (ValueArray as) -> Right $
      ValueFun $ \ (ValueArray bs) -> do
        vs <- valueFun_zipWith' f as bs
        return $ ValueArray vs

valueFun_zipWith' :: (Value -> Either String Value) -> [Value] -> [Value] -> Either String [Value]
valueFun_zipWith' z (a:as) (b:bs) = do
  ValueFun za <- z a
  zab <- za b
  zabs <- valueFun_zipWith' z as bs
  return $ zab:zabs
valueFun_zipWith' _ _ _ = return []

valueFun_zipWith3 :: Value
valueFun_zipWith3 =
  ValueFun $ \ (ValueFun f) -> Right $
    ValueFun $ \ (ValueArray as) -> Right $
      ValueFun $ \ (ValueArray bs) -> Right $
        ValueFun $ \ (ValueArray cs) -> do
          vs <- valueFun_zipWith3' f as bs cs
          return $ ValueArray vs

valueFun_zipWith3' :: (Value -> Either String Value) -> [Value] -> [Value] -> [Value] -> Either String [Value]
valueFun_zipWith3' z (a:as) (b:bs) (c:cs) = do
  ValueFun za <- z a
  ValueFun zab <- za b
  zabc <- zab c
  zabcs <- valueFun_zipWith3' z as bs cs
  return $ zabc:zabcs
valueFun_zipWith3' _ _ _ _= return []


-- Texture functions.

valueSample1D :: Value
valueSample1D =
  ValueFun $ \ (ValueTexture1D i) -> Right $
    ValueFun $ \ (ValueArray [ValueDFReal x]) -> do
    let submit = DFSample1D i x
    return $ ValueArray [
      ValueDFReal $ DFRealGetTexR submit,
      ValueDFReal $ DFRealGetTexG submit,
      ValueDFReal $ DFRealGetTexB submit,
      ValueDFReal $ DFRealGetTexA submit
      ]

valueSample2D :: Value
valueSample2D =
  ValueFun $ \ (ValueTexture2D i) -> Right $
    ValueFun $ \ (ValueArray [ValueDFReal x, ValueDFReal y]) -> do
    let submit = DFSample2D i x y
    return $ ValueArray [
      ValueDFReal $ DFRealGetTexR submit,
      ValueDFReal $ DFRealGetTexG submit,
      ValueDFReal $ DFRealGetTexB submit,
      ValueDFReal $ DFRealGetTexA submit
      ]

valueSample3D :: Value
valueSample3D =
  ValueFun $ \ (ValueTexture3D i) -> Right $
    ValueFun $ \ (ValueArray [ValueDFReal x, ValueDFReal y, ValueDFReal z]) -> do
    let submit = DFSample3D i x y z
    return $ ValueArray [
      ValueDFReal $ DFRealGetTexR submit,
      ValueDFReal $ DFRealGetTexG submit,
      ValueDFReal $ DFRealGetTexB submit,
      ValueDFReal $ DFRealGetTexA submit
      ]

valueSampleCube :: Value
valueSampleCube =
  ValueFun $ \ (ValueTextureCube i) -> Right $
    ValueFun $ \ (ValueArray [ValueDFReal x, ValueDFReal y, ValueDFReal z]) -> do
    let submit = DFSampleCube i x y z
    return $ ValueArray [
      ValueDFReal $ DFRealGetTexR submit,
      ValueDFReal $ DFRealGetTexG submit,
      ValueDFReal $ DFRealGetTexB submit,
      ValueDFReal $ DFRealGetTexA submit
      ]


-- (fixity, identifier, type scheme, desc, args different to GLSL?, arg list, definition)
library :: [(Fixity, String, String, String, Bool, [String], Value)]
library = [
  (Prefix, show OpScalarNeg, "Real -> Real", "scalar negate (as desugared from \"-\")", False, ["x"], makeValueFun1 unValueDFReal DFRealNeg ValueDFReal),
  (Prefix, show OpVectorNeg, "Real 'n -> Real 'n", "vector negate (component-wise) (as desugared from \"--\")", False, ["x"], undefined),
  (Prefix, show OpNot, "Bool -> Bool", "logical not", False, ["x"], makeValueFun1 unValueDFBool DFBoolNot ValueDFBool),
  (InfixL, show OpSubscript, "'a 'n -> Real -> 'a", "subscript (if n is statically determinable, bounds checking occurs; if n is not statically determinable, there is no bounds check, and the left argument must be a uniform)", False, ["as", "n"], undefined),
  (InfixL, show OpSwizzle, "'a 'n -> Real 'm -> 'a 'm", "swizzle, return vector of all subscripts supplied (all ns must be statically determinable; bounds checking occurs)", False, ["as", "ns"], undefined),
  (InfixL, show OpScalarAdd, "Real -> Real -> Real", "scalar add", False, ["x", "y"], makeValueFun2 unValueDFReal DFRealAdd ValueDFReal),
  (InfixL, show OpScalarSub, "Real -> Real -> Real", "scalar sub", False, ["x", "y"], makeValueFun2 unValueDFReal DFRealSub ValueDFReal),
  (InfixL, show OpScalarMul, "Real -> Real -> Real", "scalar mul", False, ["x", "y"], makeValueFun2 unValueDFReal DFRealMul ValueDFReal),
  (InfixL, show OpScalarDiv, "Real -> Real -> Real", "scalar div", False, ["x", "y"], makeValueFun2 unValueDFReal DFRealDiv ValueDFReal),
  (InfixL, show OpVectorAdd, "Real 'n -> Real 'n -> Real 'n", "vector add (component-wise)", False, ["x", "y"], undefined),
  (InfixL, show OpVectorSub, "Real 'n -> Real 'n -> Real 'n", "vector sub (component-wise)", False, ["x", "y"], undefined),
  (InfixL, show OpVectorMul, "Real 'n -> Real 'n -> Real 'n", "vector mul (component-wise)", False, ["x", "y"], undefined),
  (InfixL, show OpVectorDiv, "Real 'n -> Real 'n -> Real 'n", "vector div (component-wise)", False, ["x", "y"], undefined),
  (InfixL, show OpVectorScalarMul, "Real 'n -> Real -> Real 'n", "vector-scalar mul", False, ["x", "y"], undefined),
  (InfixL, show OpVectorScalarDiv, "Real 'n -> Real -> Real 'n", "vector-scalar div", False, ["x", "y"], undefined),
  (InfixL, show OpMatrixMatrixLinearMul, "Real 'q 'p -> Real 'r 'q -> Real 'r 'p", "matrix-matrix linear algebraic mul", False, ["x", "y"], undefined),
  (InfixR, show OpMatrixVectorLinearMul, "Real 'q 'p -> Real 'q -> Real 'p", "matrix-vector linear algebraic mul", False, ["x", "y"], undefined),
  (InfixL, show OpVectorMatrixLinearMul, "Real 'q -> Real 'r 'q -> Real 'r", "vector-matrix linear algebraic mul", False, ["x", "y"], undefined),
  (InfixN, show OpLessThan, "Real -> Real -> Bool", "less than", False, ["x", "y"], makeValueFun2 unValueDFReal DFBoolLessThan ValueDFBool),
  (InfixN, show OpLessThanEqual, "Real -> Real -> Bool", "less than or equal", False, ["x", "y"], makeValueFun2 unValueDFReal DFBoolLessThanEqual ValueDFBool),
  (InfixN, show OpGreaterThan, "Real -> Real -> Bool", "greater than", False, ["x", "y"], makeValueFun2 unValueDFReal DFBoolGreaterThan ValueDFBool),
  (InfixN, show OpGreaterThanEqual, "Real -> Real -> Bool", "greater than or equal", False, ["x", "y"], makeValueFun2 unValueDFReal DFBoolGreaterThanEqual ValueDFBool),
  (InfixN, show OpEqual, "'a -> 'a -> Bool", "equality", False, ["x", "y"], undefined),
  (InfixN, show OpNotEqual, "'a -> 'a -> Bool", "inequality", False, ["x", "y"], undefined),
  (InfixL, show OpAnd, "Bool -> Bool -> Bool", "logical and", False, ["x", "y"], makeValueFun2 unValueDFBool DFBoolAnd ValueDFBool),
  (InfixL, show OpOr, "Bool -> Bool -> Bool", "logical or", False, ["x", "y"], makeValueFun2 unValueDFBool DFBoolOr ValueDFBool),
  (Postfix, show OpTranspose, "'a 'q 'p -> 'a 'p 'q", "transpose", False, ["x"], undefined),
  (Prefix, "map", "('a -> 'b) -> 'a 'n -> 'b 'n", "map function onto array", False, ["f", "as"], valueFun_map),
  (Prefix, "foldl", "('a -> 'a -> 'b) -> 'a -> 'b 'n -> 'a", "left fold", False, ["f", "z", "bs"], valueFun_foldl),
  (Prefix, "foldr", "('a -> 'b -> 'b) -> 'b -> 'a 'n -> 'b", "right fold", False, ["f", "z", "as"], valueFun_foldr),
  (Prefix, "unroll", "('a -> 'a) -> Real -> 'a -> 'a", "apply f n times to z (n must be statically determinable)", False, ["f", "n", "z"], valueFun_unroll),
  (Prefix, "zipWith", "('a -> 'b -> 'c) -> 'a 'n -> 'b 'n -> 'c 'n", "general zip over 2 arrays", False, ["f", "as", "bs"], valueFun_zipWith),
  (Prefix, "zipWith3", "('a -> 'b -> 'c -> 'd) -> 'a 'n -> 'b 'n -> 'c 'n -> 'd 'n", "general zip over 3 arrays", False, ["f", "as", "bs", "cs"], valueFun_zipWith3),
  (Prefix, "sum", "Real 'p -> Real", "sum of components", False, ["x"], undefined),
  (Prefix, "product", "Real 'p -> Real", "product of components", False, ["x"], undefined),
  (Prefix, "any", "Bool 'p -> Bool", "logical or of components", False, ["x"], undefined),
  (Prefix, "all", "Bool 'p -> Bool", "logical and of components", False, ["x"], undefined),
  (Prefix, "sin", "Real -> Real", "sine (radians)", False, ["a"], makeValueFun1 unValueDFReal DFRealSin ValueDFReal),
  (Prefix, "cos", "Real -> Real", "cosine (radians)", False, ["a"], makeValueFun1 unValueDFReal DFRealCos ValueDFReal),
  (Prefix, "tan", "Real -> Real", "tangent (radians)", False, ["a"], makeValueFun1 unValueDFReal DFRealTan ValueDFReal),
  (Prefix, "asin", "Real -> Real", "arcsine (radians)", False, ["x"], makeValueFun1 unValueDFReal DFRealASin ValueDFReal),
  (Prefix, "acos", "Real -> Real", "arccosine (radians)", False, ["x"], makeValueFun1 unValueDFReal DFRealACos ValueDFReal),
  (Prefix, "atan", "Real -> Real -> Real", "arctangent (radians)", False, ["x", "y"], makeValueFun2 unValueDFReal DFRealATan ValueDFReal),
  (Prefix, "pow", "Real -> Real -> Real", "power", False, ["x", "y"], makeValueFun2 unValueDFReal DFRealPow ValueDFReal),
  (Prefix, "exp", "Real -> Real", "power (base e)", False, ["x"], makeValueFun1 unValueDFReal DFRealExp ValueDFReal),
  (Prefix, "exp2", "Real -> Real", "power (base 2)", False, ["x"], makeValueFun1 unValueDFReal DFRealExp2 ValueDFReal),
  (Prefix, "log", "Real -> Real", "logarithm (base e)", False, ["x"], makeValueFun1 unValueDFReal DFRealLog ValueDFReal),
  (Prefix, "log2", "Real -> Real", "logarithm (base 2)", False, ["x"], makeValueFun1 unValueDFReal DFRealLog2 ValueDFReal),
  (Prefix, "sqrt", "Real -> Real", "square root", False, ["x"], undefined),
  (Prefix, "rsqrt", "Real -> Real", "reciprocal square root", False, ["x"], makeValueFun1 unValueDFReal DFRealRsq ValueDFReal),
  (Prefix, "abs", "Real -> Real", "absolute value", False, ["x"], makeValueFun1 unValueDFReal DFRealAbs ValueDFReal),
  (Prefix, "floor", "Real -> Real", "round to negative infinity", False, ["x"], makeValueFun1 unValueDFReal DFRealFloor ValueDFReal),
  (Prefix, "ceiling", "Real -> Real", "round to positive infinity", False, ["x"], makeValueFun1 unValueDFReal DFRealCeiling ValueDFReal),
  (Prefix, "round", "Real -> Real", "round to nearest integer", False, ["x"], makeValueFun1 unValueDFReal DFRealRound ValueDFReal),
  (Prefix, "truncate", "Real -> Real", "round to zero", False, ["x"], makeValueFun1 unValueDFReal DFRealTruncate ValueDFReal),
  (Prefix, "fract", "Real -> Real", "fractional part", False, ["x"], makeValueFun1 unValueDFReal DFRealFract ValueDFReal),
  (Prefix, "mod", "Real -> Real -> Real", "modulus", False, ["x", "y"], undefined),
  (Prefix, "min", "Real -> Real -> Real", "minimum", False, ["x", "y"], makeValueFun2 unValueDFReal DFRealMin ValueDFReal),
  (Prefix, "max", "Real -> Real -> Real", "maximum", False, ["x", "y"], makeValueFun2 unValueDFReal DFRealMax ValueDFReal),
  (Prefix, "clamp", "Real -> Real -> Real -> Real", "clamp value to given range", True, ["low", "high", "x"], undefined),
  (Prefix, "mix", "Real -> Real -> Real -> Real", "linear interpolation", True, ["a", "x", "y"], makeValueFun3 unValueDFReal DFRealLrp ValueDFReal),
  (Prefix, "step", "Real -> Real -> Real", "unit step", False, ["edge", "x"], undefined),
  (Prefix, "smoothstep", "Real -> Real -> Real -> Real", "hermite interpolation", False, ["edge0", "edge1", "x"], undefined),
  (Prefix, "length", "Real 'n -> Real", "vector length (Pythagorean)", False, ["x"], undefined),
  (Prefix, "dot", "Real 'n -> Real 'n -> Real", "dot product", False, ["x", "y"], undefined),
  (Prefix, "cross", "Real 3 -> Real 3 -> Real 3", "cross product", False, ["x", "y"], undefined),
  (Prefix, "normalize", "Real 'n -> Real 'n", "normalize", False, ["x"], undefined),
  (Prefix, "faceforward", "Real 'n -> Real 'n -> Real 'n", "returns N facing forward", True, ["Nref", "I", "N"], undefined),
  (Prefix, "reflect", "Real 'n -> Real 'n -> Real 'n", "reflect I given Nref (normalized)", True, ["Nref", "I"], undefined),
  (Prefix, "refract", "Real 'n -> Real 'n -> Real 'n", "refract I given Nref (normalized) and index eta", True, ["Nref", "eta", "I"], undefined),
  (Prefix, "pad", "Real 3 -> Real 4", "pads fourth component with 1.0", False, ["x"], undefined),
  (Prefix, "sample1D", "Texture1D -> Real 1 -> Real 4", "sample 1D texture", False, ["tex", "coord"], valueSample1D),
  (Prefix, "sample2D", "Texture2D -> Real 2 -> Real 4", "sample 2D texture", False, ["tex", "coord"], valueSample2D),
  (Prefix, "sample3D", "Texture3D -> Real 3 -> Real 4", "sample 3D texture", False, ["tex", "coord"], valueSample3D),
  (Prefix, "sampleCube", "TextureCube -> Real 3 -> Real 4", "sample cubic texture", False, ["tex", "coord"], valueSampleCube)
  ]
