module Library(library, queryTypeScheme) where

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
-- - a map from library identifiers to values;
-- - the subsequent list of fresh variable references, given that some were used
--   in constructing the type schemes of the library functions.
library :: (SchemeEnv, ValueEnv, ([TypeVarRef], [DimVarRef]))
library =
  let { base = List.foldl' (
    \ (gamma, env, vrefs) (_, ident, tstr, _, _, _, v) ->
      case parseType vrefs (ByteString.pack tstr) of
        Right (t, vrefs') ->
          let sigma = Scheme (fvType t) t in
            (Map.insert ident sigma gamma, Map.insert ident v env, vrefs')
        Left msg -> error $ "in constructing library function <" ++ ident ++ ">: " ++ msg
  ) (Map.empty, Map.empty, initFreshVarRefs) libraryBase } in
  List.foldl' (
    \ (gamma, env, vrefs) (_, ident, _, _, estr) ->
      case parseExpr vrefs (ByteString.pack estr) of
        Right (e, vrefs') ->
          case inferExprType gamma e vrefs' of
            Right (t, vrefs'') ->
              case interpretExpr env e of
                Right v ->
                  let sigma = Scheme (fvType t) t in
                  (Map.insert ident sigma gamma, Map.insert ident v env, vrefs'')
                Left msg -> error $ "in constructing library function <" ++ ident ++ ">: " ++ msg
            Left msg -> error $ "in constructing library function <" ++ ident ++ ">: " ++ msg
        Left msg -> error $ "in constructing library function <" ++ ident ++ ">: " ++ msg
  ) base libraryDerived


-- Queries the library, for debugging purposes.
queryTypeScheme :: String -> String
queryTypeScheme ident =
  let (gamma, _, _) = library in
    case Map.lookup ident gamma of
      Just sigma -> show sigma
      Nothing -> "not in library"


-- Lift functions; take a static and a dynamic operator.

liftRRB :: (Double -> Double -> Bool) -> (DFReal -> DFReal -> DFBool) -> Value
liftRRB sop dop =
  ValueFun $ \ (ValueDFReal df1) -> Right $
    ValueFun $ \ (ValueDFReal df2) -> Right $
      liftRRB' sop dop df1 df2

liftRRB' :: (Double -> Double -> Bool) -> (DFReal -> DFReal -> DFBool) -> DFReal -> DFReal -> Value
liftRRB' sop dop df1 df2 =
  case (df1, df2) of
    (DFRealLiteral l1, DFRealLiteral l2) -> ValueDFBool $ DFBoolLiteral $ sop l1 l2
    _ -> ValueDFBool $ dop df1 df2

liftRRR :: (Double -> Double -> Double) -> (DFReal -> DFReal -> DFReal) -> Value
liftRRR sop dop =
  ValueFun $ \ (ValueDFReal df1) -> Right $
    ValueFun $ \ (ValueDFReal df2) -> Right $
      liftRRR' sop dop df1 df2

liftRRR' :: (Double -> Double -> Double) -> (DFReal -> DFReal -> DFReal) -> DFReal -> DFReal -> Value
liftRRR' sop dop df1 df2 =
  case (df1, df2) of
    (DFRealLiteral l1, DFRealLiteral l2) -> ValueDFReal $ DFRealLiteral $ sop l1 l2
    _ -> ValueDFReal $ dop df1 df2

liftBBB :: (Bool -> Bool -> Bool) -> (DFBool -> DFBool -> DFBool) -> Value
liftBBB sop dop =
  ValueFun $ \ (ValueDFBool df1) -> Right $
    ValueFun $ \ (ValueDFBool df2) -> Right $
      liftBBB' sop dop df1 df2

liftBBB' :: (Bool -> Bool -> Bool) -> (DFBool -> DFBool -> DFBool) -> DFBool -> DFBool -> Value
liftBBB' sop dop df1 df2 =
  case (df1, df2) of
    (DFBoolLiteral l1, DFBoolLiteral l2) -> ValueDFBool $ DFBoolLiteral $ sop l1 l2
    _ -> ValueDFBool $ dop df1 df2

liftRR :: (Double -> Double) -> (DFReal -> DFReal) -> Value
liftRR sop dop =
  ValueFun $ \ (ValueDFReal df) -> Right $
    case df of
      DFRealLiteral l -> ValueDFReal $ DFRealLiteral $ sop l
      _ -> ValueDFReal $ dop df

liftBB :: (Bool -> Bool) -> (DFBool -> DFBool) -> Value
liftBB sop dop =
  ValueFun $ \ (ValueDFBool df) -> Right $
    case df of
      DFBoolLiteral l -> ValueDFBool $ DFBoolLiteral $ sop l
      _ -> ValueDFBool $ dop df


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

valueFun_foldl1 :: Value
valueFun_foldl1 =
  ValueFun $ \ (ValueFun f) -> Right $
    ValueFun $ \ (ValueArray (vb:vbs)) ->
      valueFun_foldl' f vb vbs

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

valueFun_foldr1 :: Value
valueFun_foldr1 =
  ValueFun $ \ (ValueFun f) -> Right $
    ValueFun $ \ (ValueArray (va:vas)) ->
      valueFun_foldr' f va vas

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


-- Subscript.

valueSubscript :: Value
valueSubscript =
  ValueFun $ \ a@(ValueArray vs) -> Right $
    ValueFun $ \ (ValueDFReal sub) -> do
      let len = length vs
      case sub of
        DFRealLiteral d -> do
          let idx = floor d
          if 0 <= idx && idx < len
            then return $ vs!!idx
            else throwError $ "array index (" ++ show idx ++ ") out of bounds"
        _ -> throwError $ "array index is not statically determinable" -- todo: support dynamic indexing


-- Equality and inequality functions.

valueEqual :: Value
valueEqual =
  ValueFun $ \ v1 -> Right $
    ValueFun $ \ v2 -> valueEqual' v1 v2

valueEqual' :: Value -> Value -> Either String Value
valueEqual' (ValueUnit) (ValueUnit) = return $ ValueDFBool $ DFBoolLiteral True
valueEqual' (ValueDFReal df1) (ValueDFReal df2) = return $ liftRRB' (==) DFBoolEqualReal df1 df2
valueEqual' (ValueDFBool df1) (ValueDFBool df2) = return $ liftBBB' (==) DFBoolEqualBool df1 df2
valueEqual' (ValueTexture1D i) (ValueTexture1D i') = return $ ValueDFBool $ DFBoolLiteral $ i == i'
valueEqual' (ValueTexture2D i) (ValueTexture2D i') = return $ ValueDFBool $ DFBoolLiteral $ i == i'
valueEqual' (ValueTexture3D i) (ValueTexture3D i') = return $ ValueDFBool $ DFBoolLiteral $ i == i'
valueEqual' (ValueTextureCube i) (ValueTextureCube i') = return $ ValueDFBool $ DFBoolLiteral $ i == i'
valueEqual' (ValueArray vs1) (ValueArray vs2) = do
  vs <- zipWithM valueEqual' vs1 vs2
  return $ ValueDFBool $ List.foldl1' (\x y -> unValueDFBool $ (liftBBB' (&&) DFBoolAnd) x y) $ map unValueDFBool vs
valueEqual' (ValueTuple vs1) (ValueTuple vs2) = do
  vs <- zipWithM valueEqual' vs1 vs2
  return $ ValueDFBool $ List.foldl1' (\x y -> unValueDFBool $ (liftBBB' (&&) DFBoolAnd) x y) $ map unValueDFBool vs
valueEqual' (ValueFun _) (ValueFun _) = throwError $ "equality expression would violate run time model"
valueEqual' _ _ = undefined

valueNotEqual :: Value
valueNotEqual =
  ValueFun $ \ v1 -> Right $
    ValueFun $ \ v2 -> valueNotEqual' v1 v2

valueNotEqual' :: Value -> Value -> Either String Value
valueNotEqual' (ValueUnit) (ValueUnit) = return $ ValueDFBool $ DFBoolLiteral False
valueNotEqual' (ValueDFReal df1) (ValueDFReal df2) = return $ liftRRB' (/=) DFBoolNotEqualReal df1 df2
valueNotEqual' (ValueDFBool df1) (ValueDFBool df2) = return $ liftBBB' (/=) DFBoolNotEqualBool df1 df2
valueNotEqual' (ValueTexture1D i) (ValueTexture1D i') = return $ ValueDFBool $ DFBoolLiteral $ i /= i'
valueNotEqual' (ValueTexture2D i) (ValueTexture2D i') = return $ ValueDFBool $ DFBoolLiteral $ i /= i'
valueNotEqual' (ValueTexture3D i) (ValueTexture3D i') = return $ ValueDFBool $ DFBoolLiteral $ i /= i'
valueNotEqual' (ValueTextureCube i) (ValueTextureCube i') = return $ ValueDFBool $ DFBoolLiteral $ i /= i'
valueNotEqual' (ValueArray vs1) (ValueArray vs2) = do
  vs <- zipWithM valueNotEqual' vs1 vs2
  return $ ValueDFBool $ List.foldl1' (\x y -> unValueDFBool $ (liftBBB' (||) DFBoolOr) x y) $ map unValueDFBool vs
valueNotEqual' (ValueTuple vs1) (ValueTuple vs2) = do
  vs <- zipWithM valueNotEqual' vs1 vs2
  return $ ValueDFBool $ List.foldl1' (\x y -> unValueDFBool $ (liftBBB' (||) DFBoolOr) x y) $ map unValueDFBool vs
valueNotEqual' (ValueFun _) (ValueFun _) = throwError $ "inequality expression would violate run time model"
valueNotEqual' _ _ = undefined


-- Transpose function.
valueTranspose :: Value
valueTranspose =
  ValueFun $ \ (ValueArray outer) -> Right $
    ValueArray $ map ValueArray $ List.transpose $ map (\(ValueArray inner) -> inner) outer


-- (fixity, identifier, type scheme, desc, args different to GLSL?, arg list, definition)
libraryBase :: [(Fixity, String, String, String, Bool, [String], Value)]
libraryBase = [
  (Prefix, show OpScalarNeg, "Real -> Real", "scalar negate (as desugared from \"-\")", False, ["x"], liftRR (negate) DFRealNeg),
  (Prefix, show OpNot, "Bool -> Bool", "logical not", False, ["x"], liftBB (not) DFBoolNot),
  (InfixL, show OpSubscript, "'a 'n -> Real -> 'a", "subscript", False, ["as", "n"], valueSubscript),
  (InfixL, show OpScalarAdd, "Real -> Real -> Real", "scalar add", False, ["x", "y"], liftRRR (+) DFRealAdd),
  (InfixL, show OpScalarSub, "Real -> Real -> Real", "scalar sub", False, ["x", "y"], liftRRR (-) DFRealSub),
  (InfixL, show OpScalarMul, "Real -> Real -> Real", "scalar mul", False, ["x", "y"], liftRRR (*) DFRealMul),
  (InfixL, show OpScalarDiv, "Real -> Real -> Real", "scalar div", False, ["x", "y"], liftRRR (/) DFRealDiv),
  (InfixN, show OpLessThan, "Real -> Real -> Bool", "less than", False, ["x", "y"], liftRRB (<) DFBoolLessThan),
  (InfixN, show OpLessThanEqual, "Real -> Real -> Bool", "less than or equal", False, ["x", "y"], liftRRB (<=) DFBoolLessThanEqual),
  (InfixN, show OpGreaterThan, "Real -> Real -> Bool", "greater than", False, ["x", "y"], liftRRB (>) DFBoolGreaterThan),
  (InfixN, show OpGreaterThanEqual, "Real -> Real -> Bool", "greater than or equal", False, ["x", "y"], liftRRB (>=) DFBoolGreaterThanEqual),
  (InfixN, show OpEqual, "'a -> 'a -> Bool", "equality", False, ["x", "y"], valueEqual),
  (InfixN, show OpNotEqual, "'a -> 'a -> Bool", "inequality", False, ["x", "y"], valueNotEqual),
  (InfixL, show OpAnd, "Bool -> Bool -> Bool", "logical and", False, ["x", "y"], liftBBB (&&) DFBoolAnd),
  (InfixL, show OpOr, "Bool -> Bool -> Bool", "logical or", False, ["x", "y"], liftBBB (||) DFBoolOr),
  (Postfix, show OpTranspose, "'a 'p 'q -> 'a 'q 'p", "transpose", False, ["x"], valueTranspose),
  (Prefix, "map", "('a -> 'b) -> 'a 'n -> 'b 'n", "map function onto array", False, ["f", "as"], valueFun_map),
  (Prefix, "foldl", "('a -> 'a -> 'b) -> 'a -> 'b 'n -> 'a", "left fold", False, ["f", "z", "bs"], valueFun_foldl),
  (Prefix, "foldl1", "('a -> 'a -> 'a) -> 'a 'n -> 'a", "left fold without initial accumulator", False, ["f", "as"], valueFun_foldl1),
  (Prefix, "foldr", "('a -> 'b -> 'b) -> 'b -> 'a 'n -> 'b", "right fold", False, ["f", "z", "as"], valueFun_foldr),
  (Prefix, "foldr1", "('a -> 'a -> 'a) -> 'a 'n -> 'a", "right fold without initial accumulator", False, ["f", "as"], valueFun_foldr1),
  (Prefix, "unroll", "('a -> 'a) -> Real -> 'a -> 'a", "apply f n times to z (n must be statically determinable)", False, ["f", "n", "z"], valueFun_unroll),
  (Prefix, "zipWith", "('a -> 'b -> 'c) -> 'a 'n -> 'b 'n -> 'c 'n", "general zip over 2 arrays", False, ["f", "as", "bs"], valueFun_zipWith),
  (Prefix, "zipWith3", "('a -> 'b -> 'c -> 'd) -> 'a 'n -> 'b 'n -> 'c 'n -> 'd 'n", "general zip over 3 arrays", False, ["f", "as", "bs", "cs"], valueFun_zipWith3),
  (Prefix, "pi", "Real", "pi", False, [], ValueDFReal $ DFRealLiteral pi),
  (Prefix, "sin", "Real -> Real", "sine (radians)", False, ["a"], liftRR sin DFRealSin),
  (Prefix, "cos", "Real -> Real", "cosine (radians)", False, ["a"], liftRR cos DFRealCos),
  (Prefix, "tan", "Real -> Real", "tangent (radians)", False, ["a"], liftRR tan DFRealTan),
  (Prefix, "asin", "Real -> Real", "arcsine (radians)", False, ["x"], liftRR asin DFRealASin),
  (Prefix, "acos", "Real -> Real", "arccosine (radians)", False, ["x"], liftRR acos DFRealACos),
  (Prefix, "atan", "Real -> Real -> Real", "arctangent (radians)", False, ["x", "y"], liftRR atan DFRealATan),
  (Prefix, "pow", "Real -> Real -> Real", "power", False, ["x", "y"], liftRRR (**) DFRealPow),
  (Prefix, "exp", "Real -> Real", "power (base e)", False, ["x"], liftRR exp DFRealExp),
  (Prefix, "exp2", "Real -> Real", "power (base 2)", False, ["x"], liftRR (2**) DFRealExp2),
  (Prefix, "log", "Real -> Real", "logarithm (base e)", False, ["x"], liftRR log DFRealLog),
  (Prefix, "log2", "Real -> Real", "logarithm (base 2)", False, ["x"], liftRR (logBase 2) DFRealLog2),
  (Prefix, "rsqrt", "Real -> Real", "reciprocal square root", False, ["x"], liftRR (\x -> 1 / sqrt x) DFRealRsq),
  (Prefix, "abs", "Real -> Real", "absolute value", False, ["x"], liftRR abs DFRealAbs),
  (Prefix, "floor", "Real -> Real", "round to negative infinity", False, ["x"], liftRR (fromIntegral . floor) DFRealFloor),
  (Prefix, "ceiling", "Real -> Real", "round to positive infinity", False, ["x"], liftRR (fromIntegral . ceiling) DFRealCeiling),
  (Prefix, "round", "Real -> Real", "round to nearest integer", False, ["x"], liftRR (fromIntegral . round) DFRealRound),
  (Prefix, "truncate", "Real -> Real", "round to zero", False, ["x"], liftRR (fromIntegral . truncate) DFRealTruncate),
  (Prefix, "fract", "Real -> Real", "fractional part", False, ["x"], liftRR (snd . properFraction) DFRealFract),
  (Prefix, "min", "Real -> Real -> Real", "minimum", False, ["x", "y"], liftRRR min DFRealMin),
  (Prefix, "max", "Real -> Real -> Real", "maximum", False, ["x", "y"], liftRRR max DFRealMax),
  (Prefix, "sample1D", "Texture1D -> Real 1 -> Real 4", "sample 1D texture", False, ["tex", "coord"], valueSample1D),
  (Prefix, "sample2D", "Texture2D -> Real 2 -> Real 4", "sample 2D texture", False, ["tex", "coord"], valueSample2D),
  (Prefix, "sample3D", "Texture3D -> Real 3 -> Real 4", "sample 3D texture", False, ["tex", "coord"], valueSample3D),
  (Prefix, "sampleCube", "TextureCube -> Real 3 -> Real 4", "sample cubic texture", False, ["tex", "coord"], valueSampleCube)
  ]

-- (fixity, identifier, type scheme, desc, args different to GLSL?, arg list, definition)
libraryDerived :: [(Fixity, String, String, Bool, String)]
libraryDerived = [
  (InfixR, show OpApply, "function application operator", False, "\\f x. f x"),
  (Prefix, show OpVectorNeg, "vector negate (component-wise) (as desugared from \"--\")", False, "map negate"),
  (InfixL, show OpSwizzle, "swizzle", False, "\\as ns. map ((!) as) ns"),
  (InfixL, show OpVectorAdd, "vector add (component-wise)", False, "zipWith (+)"),
  (InfixL, show OpVectorSub, "vector sub (component-wise)", False, "zipWith (-)"),
  (InfixL, show OpVectorMul, "vector mul (component-wise)", False, "zipWith (*)"),
  (InfixL, show OpVectorDiv, "vector div (component-wise)", False, "zipWith (/)"),
  (InfixL, show OpVectorScalarMul, "vector-scalar mul", False, "\\xs y. map (\\x. x * y) xs"),
  (InfixL, show OpVectorScalarDiv, "vector-scalar div", False, "\\xs y. map (\\x. x / y) xs"),
  (Prefix, "sum", "sum of components", False, "foldl1 (+)"),
  (Prefix, "product", "product of components", False, "foldl1 (*)"),
  (Prefix, "any", "logical or of components", False, "foldl1 (||)"),
  (Prefix, "all", "logical and of components", False, "foldl1 (&&)"),
  (Prefix, "sqrt", "square root", False, "\\x. 1 / rsqrt x"),
  (Prefix, "mod", "modulus", False, "\\x y. x - y * floor (x/y)"),
  (Prefix, "dot", "dot product", False, "\\x y. sum $ x ** y"),
  (Prefix, "cross", "cross product", False, "\\[x1, x2, x3] [y1, y2, y3]. [x2 * y3 - x3 * y2, x3 * y1 - x1 * y3, x1 * y2 - x2 * y1]"),
  (Prefix, "length", "vector length (Pythagorean)", False, "\\x. sqrt $ dot x x"),
  (Prefix, "normalize", "normalize", False, "\\x. x **. (rsqrt $ dot x x)"),
  (InfixR, show OpMatrixVectorLinearMul, "matrix-vector linear algebraic mul", False, "\\m v. map (dot v) m"),
  (InfixL, show OpVectorMatrixLinearMul, "vector-matrix linear algebraic mul", False, "\\v m. m' #. v"),
  (InfixL, show OpMatrixMatrixLinearMul, "matrix-matrix linear algebraic mul", False, "\\ma mb. (map ((#.) ma) (mb'))'"),
  (Prefix, "clamp", "clamp value to given range", True, "\\low high x. min (max x low) high"),
  (Prefix, "step", "unit step", False, "\\edge x. if x < edge then 0 else 1"),
  (Prefix, "mix", "linear interpolation", True, "\\a x y. x * (1 - a) + y * a"),
  (Prefix, "smoothstep", "hermite interpolation", False, "\\edge0 edge1 x. let t = clamp 0 1 ((x - edge0) / (edge1 - edge0)) in t * t * (3 - 2 * t)"),
  (Prefix, "faceforward", "returns N facing forward", True, "\\Nref I N. if dot Nref I < 0 then N else --N"),
  (Prefix, "reflect", "reflect I given Nref (normalized)", True, "\\Nref I. I -- Nref **. (2 * dot Nref I)"),
  (Prefix, "refract", "refract I given Nref (normalized) and index eta", True, "\\Nref eta I. let d = dot Nref I in let eta2 = eta * eta in let k = 1 - eta2 + eta2 * d * d in if k < 0 then map (\\_. 0) Nref else I **. eta -- Nref **. (eta * d + sqrt k)"),
  (Prefix, "pad", "pads fourth component with 1.0", False, "\\[x1, x2, x3]. [x1, x2, x3, 1.0]")
  ]
