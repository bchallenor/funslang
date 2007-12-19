module Library(libraryTypeSchemes, libraryValues, queryTypeScheme) where

import qualified Data.ByteString.Lazy.Char8 as ByteString
import qualified Data.List as List
import qualified Data.Map as Map

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
  (Prefix, "map", "('a -> 'b) -> 'a 'n -> 'b 'n", "map function onto array", False, ["f", "as"], undefined),
  (Prefix, "foldl", "('a -> 'a -> 'b) -> 'a -> 'b 'n -> 'a", "left fold", False, ["f", "z", "bs"], undefined),
  (Prefix, "foldr", "('a -> 'b -> 'b) -> 'b -> 'a 'n -> 'b", "right fold", False, ["f", "z", "as"], undefined),
  (Prefix, "unroll", "('a -> 'a) -> Real -> 'a -> 'a", "apply f n times to z (n must be statically determinable)", False, ["f", "n", "z"], undefined),
  (Prefix, "zipWith", "('a -> 'b -> 'c) -> 'a 'n -> 'b 'n -> 'c 'n", "general zip over 2 arrays", False, ["f", "as", "bs"], undefined),
  (Prefix, "zipWith3", "('a -> 'b -> 'c -> 'd) -> 'a 'n -> 'b 'n -> 'c 'n -> 'd 'n", "general zip over 3 arrays", False, ["f", "as", "bs", "cs"], undefined),
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
  (Prefix, "sample1D", "Texture1D -> Real 1 -> Real 4", "sample 1D texture", False, ["tex", "coord"], undefined),
  (Prefix, "sample2D", "Texture2D -> Real 2 -> Real 4", "sample 2D texture", False, ["tex", "coord"], undefined),
  (Prefix, "sample3D", "Texture3D -> Real 3 -> Real 4", "sample 3D texture", False, ["tex", "coord"], undefined),
  (Prefix, "sampleCube", "TextureCube -> Real 3 -> Real 4", "sample cubic texture", False, ["tex", "coord"], undefined)
  ]
