module Library() where

import Typing
import Pretty
import Lexer
import Parser
import Representation


data Fixity
  = Prefix
  | InfixL
  | InfixR
  | InfixN
  | Postfix
  
  deriving (Show, Eq)


-- Parses the given type string, and generalizes it as if let-bound.
sig :: String -> Scheme
sig tstr =
  let t = typeFromExType typeVarRefs dimVarRefs $ parseExType $ lexer tstr in
    Scheme (fv t) t


-- (fixity, identifier, type scheme, desc, args different to GLSL?, arg list)
library :: [(Fixity, String, Scheme, String, Bool, [String])]
library = [
  (Prefix, show OpScalarNeg, sig "Real -> Real", "scalar negate (as desugared from \"-\")", False, ["x"]),
  (Prefix, show OpVectorNeg, sig "Real n -> Real n", "vector negate (component-wise) (as desugared from \"--\")", False, ["x"]),
  (Prefix, show OpNot, sig "Bool -> Bool", "logical not", False, ["x"]),
  (InfixL, show OpSubscript, sig "a n -> Real -> 'a", "subscript (if n is statically determinable, bounds checking occurs; if n is not statically determinable, there is no bounds check, and the left argument must be a uniform)", False, ["as", "n"]),
  (InfixL, show OpSwizzle, sig "a n -> Real m -> 'a m", "swizzle, return vector of all subscripts supplied (if ns are all statically determinable, bounds checking occurs; if any n is not statically determinable, there is no bounds check, and the left argument must be a uniform) ", False, ["as", "ns"]),
  (InfixL, show OpScalarAdd, sig "Real -> Real -> Real", "scalar add", False, ["x", "y"]),
  (InfixL, show OpScalarSub, sig "Real -> Real -> Real", "scalar sub", False, ["x", "y"]),
  (InfixL, show OpScalarMul, sig "Real -> Real -> Real", "scalar mul", False, ["x", "y"]),
  (InfixL, show OpScalarDiv, sig "Real -> Real -> Real", "scalar div", False, ["x", "y"]),
  (InfixL, show OpVectorAdd, sig "Real -> Real -> Real", "vector add (component-wise)", False, ["x", "y"]),
  (InfixL, show OpVectorSub, sig "Real -> Real -> Real", "vector sub (component-wise)", False, ["x", "y"]),
  (InfixL, show OpVectorMul, sig "Real -> Real -> Real", "vector mul (component-wise)", False, ["x", "y"]),
  (InfixL, show OpVectorDiv, sig "Real -> Real -> Real", "vector div (component-wise)", False, ["x", "y"]),
  (InfixL, show OpVectorScalarMul, sig "Real p -> Real -> Real p", "vector-scalar mul", False, ["x", "y"]),
  (InfixL, show OpVectorScalarDiv, sig "Real p -> Real -> Real p", "vector-scalar div", False, ["x", "y"]),
  (InfixL, show OpMatrixMatrixLinearMul, sig "Real q p -> Real r q -> Real r p", "matrix-matrix linear algebraic mul", False, ["x", "y"]),
  (InfixR, show OpMatrixVectorLinearMul, sig "Real q p -> Real q -> Real p", "matrix-vector linear algebraic mul", False, ["x", "y"]),
  (InfixL, show OpVectorMatrixLinearMul, sig "Real q -> Real r q -> Real r", "vector-matrix linear algebraic mul", False, ["x", "y"]),
  (InfixN, show OpLessThan, sig "Real -> Real -> Bool", "less than", False, ["x", "y"]),
  (InfixN, show OpLessThanEqual, sig "Real -> Real -> Bool", "less than or equal", False, ["x", "y"]),
  (InfixN, show OpGreaterThan, sig "Real -> Real -> Bool", "greater than", False, ["x", "y"]),
  (InfixN, show OpGreaterThanEqual, sig "Real -> Real -> Bool", "greater than or equal", False, ["x", "y"]),
  (InfixN, show OpEqual, sig "'a -> 'a -> Bool", "equality", False, ["x", "y"]),
  (InfixN, show OpNotEqual, sig "'a -> 'a -> Bool", "inequality", False, ["x", "y"]),
  (InfixL, show OpAnd, sig "Bool -> Bool -> Bool", "logical and", False, ["x", "y"]),
  (InfixL, show OpOr, sig "Bool -> Bool -> Bool", "logical or", False, ["x", "y"]),
  (Postfix, show OpTranspose, sig "'a q p -> 'a p q", "transpose", False, ["x"]),
  (Prefix, "map", sig "('a -> 'b) -> 'a n -> 'b n", "map function onto array", False, ["f", "as"]),
  (Prefix, "foldl", sig "('a -> 'a -> 'b) -> 'a -> 'b n -> 'a", "left fold", False, ["f", "z", "bs"]),
  (Prefix, "foldr", sig "('a -> 'b -> 'b) -> 'b -> 'a n -> 'b", "right fold", False, ["f", "z", "as"]),
  (Prefix, "unroll", sig "('a -> 'a) -> Real -> 'a -> 'a", "apply f n times to z (n must be statically determinable)", False, ["f", "n", "z"]),
  (Prefix, "zipWith", sig "('a -> 'b -> 'c) -> 'a n -> 'b n -> 'c n", "general zip over 2 arrays", False, ["f", "as", "bs"]),
  (Prefix, "zipWith3", sig "('a -> 'b -> 'c -> 'd) -> 'a n -> 'b n -> 'c n -> 'd n", "general zip over 3 arrays", False, ["f", "as", "bs", "cs"]),
  (Prefix, "sum", sig "Real p -> Real", "sum of components", False, ["x"]),
  (Prefix, "product", sig "Real p -> Real", "product of components", False, ["x"]),
  (Prefix, "any", sig "Bool p -> Bool", "logical or of components", False, ["x"]),
  (Prefix, "all", sig "Bool p -> Bool", "logical and of components", False, ["x"]),
  (Prefix, "sin", sig "Real -> Real", "sine (radians)", False, ["a"]),
  (Prefix, "cos", sig "Real -> Real", "cosine (radians)", False, ["a"]),
  (Prefix, "tan", sig "Real -> Real", "tangent (radians)", False, ["a"]),
  (Prefix, "asin", sig "Real -> Real", "arcsine (radians)", False, ["x"]),
  (Prefix, "acos", sig "Real -> Real", "arccosine (radians)", False, ["x"]),
  (Prefix, "atan", sig "Real -> Real -> Real", "arctangent (radians)", False, ["x", "y"]),
  (Prefix, "pow", sig "Real -> Real -> Real", "power", False, ["x", "y"]),
  (Prefix, "exp", sig "Real -> Real", "power (base e)", False, ["x"]),
  (Prefix, "exp2", sig "Real -> Real", "power (base 2)", False, ["x"]),
  (Prefix, "log", sig "Real -> Real", "logarithm (base e)", False, ["x"]),
  (Prefix, "log2", sig "Real -> Real", "logarithm (base 2)", False, ["x"]),
  (Prefix, "sqrt", sig "Real -> Real", "square root", False, ["x"]),
  (Prefix, "rsqrt", sig "Real -> Real", "reciprocal square root", False, ["x"]),
  (Prefix, "abs", sig "Real -> Real", "absolute value", False, ["x"]),
  (Prefix, "floor", sig "Real -> Real", "round to negative infinity", False, ["x"]),
  (Prefix, "ceil", sig "Real -> Real", "round to positive infinity", False, ["x"]),
  (Prefix, "round", sig "Real -> Real", "round to nearest integer", False, ["x"]),
  (Prefix, "truncate", sig "Real -> Real", "round to zero", False, ["x"]),
  (Prefix, "fract", sig "Real -> Real", "fractional part", False, ["x"]),
  (Prefix, "mod", sig "Real -> Real -> Real", "modulus", False, ["x", "y"]),
  (Prefix, "min", sig "Real -> Real -> Real", "minimum", False, ["x", "y"]),
  (Prefix, "max", sig "Real -> Real -> Real", "maximum", False, ["x", "y"]),
  (Prefix, "clamp", sig "Real -> Real -> Real -> Real", "clamp value to given range", True, ["low", "high", "x"]),
  (Prefix, "mix", sig "Real -> Real -> Real -> Real", "linear interpolation", True, ["a", "x", "y"]),
  (Prefix, "step", sig "Real -> Real -> Real", "unit step", False, ["edge", "x"]),
  (Prefix, "smoothstep", sig "Real -> Real -> Real -> Real", "hermite interpolation", False, ["edge0", "edge1", "x"]),
  (Prefix, "length", sig "Real n -> Real", "vector length (Pythagorean)", False, ["x"]),
  (Prefix, "dot", sig "Real n -> Real n -> Real", "dot product", False, ["x", "y"]),
  (Prefix, "cross", sig "Real 3 -> Real 3 -> Real 3", "cross product", False, ["x", "y"]),
  (Prefix, "normalize", sig "Real n -> Real n", "normalize", False, ["x"]),
  (Prefix, "faceforward", sig "Real n -> Real n -> Real n", "returns N facing forward", True, ["Nref", "I", "N"]),
  (Prefix, "reflect", sig "Real n -> Real n -> Real n", "reflect I given Nref (normalized)", True, ["Nref", "I"]),
  (Prefix, "refract", sig "Real n -> Real n -> Real n", "refract I given Nref (normalized) and index eta", True, ["Nref", "eta", "I"]),
  (Prefix, "sample1D", sig "Texture1D -> Real 1 -> Real 4", "sample 1D texture", False, ["tex", "coord"]),
  (Prefix, "sample2D", sig "Texture2D -> Real 2 -> Real 4", "sample 2D texture", False, ["tex", "coord"]),
  (Prefix, "sample3D", sig "Texture3D -> Real 3 -> Real 4", "sample 3D texture", False, ["tex", "coord"]),
  (Prefix, "sampleCube", sig "TextureCube -> Real 3 -> Real 4", "sample cubic texture", False, ["tex", "coord"])
  ]
