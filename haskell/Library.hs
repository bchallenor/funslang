module Library(initLibrary, queryLibrary) where

import qualified Data.ByteString.Lazy.Char8 as ByteString
import qualified Data.List as List
import qualified Data.Map as Map

import Typing
import Parser
import Representation


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
initLibrary :: (Map.Map String Scheme, ([TypeVarRef], [DimVarRef]))
initLibrary =
    List.foldl' (
      \ (env, vrefs) (_, ident, tstr, _, _, _) ->
        case parseType vrefs (ByteString.pack tstr) of
          POk PState{fresh_vrefs=vrefs'} t ->
            let sigma = Scheme (fv t) t in
              (Map.insert ident sigma env, vrefs')
          PFailed s msg -> error $ "library function type does not parse: " ++ msg
    ) (Map.empty, initFreshVarRefs) library


-- Queries the library, for debugging purposes.
queryLibrary :: String -> String
queryLibrary ident =
  let (m, _) = initLibrary in
    case Map.lookup ident m of
      Just sigma -> show sigma
      Nothing -> "not in library"


-- (fixity, identifier, type scheme, desc, args different to GLSL?, arg list)
library :: [(Fixity, String, String, String, Bool, [String])]
library = [
  (Prefix, show OpScalarNeg, "Real -> Real", "scalar negate (as desugared from \"-\")", False, ["x"]),
  (Prefix, show OpVectorNeg, "Real 'n -> Real 'n", "vector negate (component-wise) (as desugared from \"--\")", False, ["x"]),
  (Prefix, show OpNot, "Bool -> Bool", "logical not", False, ["x"]),
  (InfixL, show OpSubscript, "'a 'n -> Real -> 'a", "subscript (if n is statically determinable, bounds checking occurs; if n is not statically determinable, there is no bounds check, and the left argument must be a uniform)", False, ["as", "n"]),
  (InfixL, show OpSwizzle, "'a 'n -> Real 'm -> 'a 'm", "swizzle, return vector of all subscripts supplied (if ns are all statically determinable, bounds checking occurs; if any n is not statically determinable, there is no bounds check, and the left argument must be a uniform) ", False, ["as", "ns"]),
  (InfixL, show OpScalarAdd, "Real -> Real -> Real", "scalar add", False, ["x", "y"]),
  (InfixL, show OpScalarSub, "Real -> Real -> Real", "scalar sub", False, ["x", "y"]),
  (InfixL, show OpScalarMul, "Real -> Real -> Real", "scalar mul", False, ["x", "y"]),
  (InfixL, show OpScalarDiv, "Real -> Real -> Real", "scalar div", False, ["x", "y"]),
  (InfixL, show OpVectorAdd, "Real 'n -> Real 'n -> Real 'n", "vector add (component-wise)", False, ["x", "y"]),
  (InfixL, show OpVectorSub, "Real 'n -> Real 'n -> Real 'n", "vector sub (component-wise)", False, ["x", "y"]),
  (InfixL, show OpVectorMul, "Real 'n -> Real 'n -> Real 'n", "vector mul (component-wise)", False, ["x", "y"]),
  (InfixL, show OpVectorDiv, "Real 'n -> Real 'n -> Real 'n", "vector div (component-wise)", False, ["x", "y"]),
  (InfixL, show OpVectorScalarMul, "Real 'n -> Real -> Real 'n", "vector-scalar mul", False, ["x", "y"]),
  (InfixL, show OpVectorScalarDiv, "Real 'n -> Real -> Real 'n", "vector-scalar div", False, ["x", "y"]),
  (InfixL, show OpMatrixMatrixLinearMul, "Real 'q 'p -> Real 'r 'q -> Real 'r 'p", "matrix-matrix linear algebraic mul", False, ["x", "y"]),
  (InfixR, show OpMatrixVectorLinearMul, "Real 'q 'p -> Real 'q -> Real 'p", "matrix-vector linear algebraic mul", False, ["x", "y"]),
  (InfixL, show OpVectorMatrixLinearMul, "Real 'q -> Real 'r 'q -> Real 'r", "vector-matrix linear algebraic mul", False, ["x", "y"]),
  (InfixN, show OpLessThan, "Real -> Real -> Bool", "less than", False, ["x", "y"]),
  (InfixN, show OpLessThanEqual, "Real -> Real -> Bool", "less than or equal", False, ["x", "y"]),
  (InfixN, show OpGreaterThan, "Real -> Real -> Bool", "greater than", False, ["x", "y"]),
  (InfixN, show OpGreaterThanEqual, "Real -> Real -> Bool", "greater than or equal", False, ["x", "y"]),
  (InfixN, show OpEqual, "'a -> 'a -> Bool", "equality", False, ["x", "y"]),
  (InfixN, show OpNotEqual, "'a -> 'a -> Bool", "inequality", False, ["x", "y"]),
  (InfixL, show OpAnd, "Bool -> Bool -> Bool", "logical and", False, ["x", "y"]),
  (InfixL, show OpOr, "Bool -> Bool -> Bool", "logical or", False, ["x", "y"]),
  (Postfix, show OpTranspose, "'a 'q 'p -> 'a 'p 'q", "transpose", False, ["x"]),
  (Prefix, "map", "('a -> 'b) -> 'a 'n -> 'b 'n", "map function onto array", False, ["f", "as"]),
  (Prefix, "foldl", "('a -> 'a -> 'b) -> 'a -> 'b 'n -> 'a", "left fold", False, ["f", "z", "bs"]),
  (Prefix, "foldr", "('a -> 'b -> 'b) -> 'b -> 'a 'n -> 'b", "right fold", False, ["f", "z", "as"]),
  (Prefix, "unroll", "('a -> 'a) -> Real -> 'a -> 'a", "apply f n times to z (n must be statically determinable)", False, ["f", "n", "z"]),
  (Prefix, "zipWith", "('a -> 'b -> 'c) -> 'a 'n -> 'b 'n -> 'c 'n", "general zip over 2 arrays", False, ["f", "as", "bs"]),
  (Prefix, "zipWith3", "('a -> 'b -> 'c -> 'd) -> 'a 'n -> 'b 'n -> 'c 'n -> 'd 'n", "general zip over 3 arrays", False, ["f", "as", "bs", "cs"]),
  (Prefix, "sum", "Real 'p -> Real", "sum of components", False, ["x"]),
  (Prefix, "product", "Real 'p -> Real", "product of components", False, ["x"]),
  (Prefix, "any", "Bool 'p -> Bool", "logical or of components", False, ["x"]),
  (Prefix, "all", "Bool 'p -> Bool", "logical and of components", False, ["x"]),
  (Prefix, "sin", "Real -> Real", "sine (radians)", False, ["a"]),
  (Prefix, "cos", "Real -> Real", "cosine (radians)", False, ["a"]),
  (Prefix, "tan", "Real -> Real", "tangent (radians)", False, ["a"]),
  (Prefix, "asin", "Real -> Real", "arcsine (radians)", False, ["x"]),
  (Prefix, "acos", "Real -> Real", "arccosine (radians)", False, ["x"]),
  (Prefix, "atan", "Real -> Real -> Real", "arctangent (radians)", False, ["x", "y"]),
  (Prefix, "pow", "Real -> Real -> Real", "power", False, ["x", "y"]),
  (Prefix, "exp", "Real -> Real", "power (base e)", False, ["x"]),
  (Prefix, "exp2", "Real -> Real", "power (base 2)", False, ["x"]),
  (Prefix, "log", "Real -> Real", "logarithm (base e)", False, ["x"]),
  (Prefix, "log2", "Real -> Real", "logarithm (base 2)", False, ["x"]),
  (Prefix, "sqrt", "Real -> Real", "square root", False, ["x"]),
  (Prefix, "rsqrt", "Real -> Real", "reciprocal square root", False, ["x"]),
  (Prefix, "abs", "Real -> Real", "absolute value", False, ["x"]),
  (Prefix, "floor", "Real -> Real", "round to negative infinity", False, ["x"]),
  (Prefix, "ceil", "Real -> Real", "round to positive infinity", False, ["x"]),
  (Prefix, "round", "Real -> Real", "round to nearest integer", False, ["x"]),
  (Prefix, "truncate", "Real -> Real", "round to zero", False, ["x"]),
  (Prefix, "fract", "Real -> Real", "fractional part", False, ["x"]),
  (Prefix, "mod", "Real -> Real -> Real", "modulus", False, ["x", "y"]),
  (Prefix, "min", "Real -> Real -> Real", "minimum", False, ["x", "y"]),
  (Prefix, "max", "Real -> Real -> Real", "maximum", False, ["x", "y"]),
  (Prefix, "clamp", "Real -> Real -> Real -> Real", "clamp value to given range", True, ["low", "high", "x"]),
  (Prefix, "mix", "Real -> Real -> Real -> Real", "linear interpolation", True, ["a", "x", "y"]),
  (Prefix, "step", "Real -> Real -> Real", "unit step", False, ["edge", "x"]),
  (Prefix, "smoothstep", "Real -> Real -> Real -> Real", "hermite interpolation", False, ["edge0", "edge1", "x"]),
  (Prefix, "length", "Real 'n -> Real", "vector length (Pythagorean)", False, ["x"]),
  (Prefix, "dot", "Real 'n -> Real 'n -> Real", "dot product", False, ["x", "y"]),
  (Prefix, "cross", "Real 3 -> Real 3 -> Real 3", "cross product", False, ["x", "y"]),
  (Prefix, "normalize", "Real 'n -> Real 'n", "normalize", False, ["x"]),
  (Prefix, "faceforward", "Real 'n -> Real 'n -> Real 'n", "returns N facing forward", True, ["Nref", "I", "N"]),
  (Prefix, "reflect", "Real 'n -> Real 'n -> Real 'n", "reflect I given Nref (normalized)", True, ["Nref", "I"]),
  (Prefix, "refract", "Real 'n -> Real 'n -> Real 'n", "refract I given Nref (normalized) and index eta", True, ["Nref", "eta", "I"]),
  (Prefix, "sample1D", "Texture1D -> Real 1 -> Real 4", "sample 1D texture", False, ["tex", "coord"]),
  (Prefix, "sample2D", "Texture2D -> Real 2 -> Real 4", "sample 2D texture", False, ["tex", "coord"]),
  (Prefix, "sample3D", "Texture3D -> Real 3 -> Real 4", "sample 3D texture", False, ["tex", "coord"]),
  (Prefix, "sampleCube", "TextureCube -> Real 3 -> Real 4", "sample cubic texture", False, ["tex", "coord"])
  ]
