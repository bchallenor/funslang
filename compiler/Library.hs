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
  let wrapmsg ident msg = "in constructing library function <" ++ ident ++ ">: " ++ msg in
  
  let { base = List.foldl' (
    \ (gamma, env, vrefs) (_, ident, tstr, _, _, _, v) ->
      case parseType vrefs (ByteString.pack tstr) of
        Right (t, vrefs') ->
          let sigma = Scheme (fvType t) t in
            (Map.insert ident sigma gamma, Map.insert ident v env, vrefs')
        Left msg -> error $ wrapmsg ident msg
  ) (Map.empty, Map.empty, initFreshVarRefs) libraryBase } in
  
  List.foldl' (
    \ (gamma, env, vrefs) (_, ident, _, _, estr) ->
      case parseExpr vrefs (ByteString.pack estr) of
        Right (e, vrefs') ->
          case inferExprType gamma e vrefs' of
            Right (t, vrefs'') ->
              let sigma = Scheme (fvType t) t in
                (Map.insert ident sigma gamma, Map.insert ident (interpretExpr env e) env, vrefs'')
            Left msg -> error $ wrapmsg ident msg
        Left msg -> error $ wrapmsg ident msg
  ) base libraryDerived


-- Queries the library, for debugging purposes.
queryTypeScheme :: String -> String
queryTypeScheme ident =
  let (gamma, _, _) = library in
    case Map.lookup ident gamma of
      Just sigma -> show sigma
      Nothing -> "not in library"


-- Lift functions; take a static and a dynamic operator.

liftRRB :: (Double -> Double -> Bool) -> (Int -> DFReal -> DFReal -> DFBool) -> InterpretM Value
liftRRB sop dop = return $
  ValueFun $ \ (ValueDFReal df1) -> return $
    ValueFun $ \ (ValueDFReal df2) ->
      liftRRB' sop dop df1 df2

liftRRB' :: (Double -> Double -> Bool) -> (Int -> DFReal -> DFReal -> DFBool) -> DFReal -> DFReal -> InterpretM Value
liftRRB' sop dop df1 df2 = do
  n <- freshNode
  case (df1, df2) of
    (DFRealLiteral _ l1, DFRealLiteral _ l2) -> return $ ValueDFBool $ DFBoolLiteral n $ sop l1 l2
    _ -> return $ ValueDFBool $ dop n df1 df2

liftRRR :: (Double -> Double -> Double) -> (Int -> DFReal -> DFReal -> DFReal) -> InterpretM Value
liftRRR sop dop = return $
  ValueFun $ \ (ValueDFReal df1) -> return $
    ValueFun $ \ (ValueDFReal df2) ->
      liftRRR' sop dop df1 df2

liftRRR' :: (Double -> Double -> Double) -> (Int -> DFReal -> DFReal -> DFReal) -> DFReal -> DFReal -> InterpretM Value
liftRRR' sop dop df1 df2 = do
  n <- freshNode
  case (df1, df2) of
    (DFRealLiteral _ l1, DFRealLiteral _ l2) -> return $ ValueDFReal $ DFRealLiteral n $ sop l1 l2
    _ -> return $ ValueDFReal $ dop n df1 df2

liftBBB :: (Bool -> Bool -> Bool) -> (Int -> DFBool -> DFBool -> DFBool) -> InterpretM Value
liftBBB sop dop = return $
  ValueFun $ \ (ValueDFBool df1) -> return $
    ValueFun $ \ (ValueDFBool df2) ->
      liftBBB' sop dop df1 df2

liftBBB' :: (Bool -> Bool -> Bool) -> (Int -> DFBool -> DFBool -> DFBool) -> DFBool -> DFBool -> InterpretM Value
liftBBB' sop dop df1 df2 = do
  n <- freshNode
  case (df1, df2) of
    (DFBoolLiteral _ l1, DFBoolLiteral _ l2) -> return $ ValueDFBool $ DFBoolLiteral n $ sop l1 l2
    _ -> return $ ValueDFBool $ dop n df1 df2

liftRR :: (Double -> Double) -> (Int -> DFReal -> DFReal) -> InterpretM Value
liftRR sop dop = do
  n <- freshNode
  return $
    ValueFun $ \ (ValueDFReal df) ->
      case df of
        DFRealLiteral _ l -> return $ ValueDFReal $ DFRealLiteral n $ sop l
        _ -> return $ ValueDFReal $ dop n df

liftBB :: (Bool -> Bool) -> (Int -> DFBool -> DFBool) -> InterpretM Value
liftBB sop dop = do
  n <- freshNode
  return $
    ValueFun $ \ (ValueDFBool df) ->
      case df of
        DFBoolLiteral _ l -> return $ ValueDFBool $ DFBoolLiteral n $ sop l
        _ -> return $ ValueDFBool $ dop n df


-- Higher order functions.

valueFun_map :: InterpretM Value
valueFun_map = return $
  ValueFun $ \ (ValueFun f) -> return $
    ValueFun $ \ (ValueArray as) -> do
      vs <- mapM f as
      return $ ValueArray vs

valueFun_foldl :: InterpretM Value
valueFun_foldl = return $
  ValueFun $ \ (ValueFun f) -> return $
    ValueFun $ \ va -> return $
      ValueFun $ \ (ValueArray vbs) ->
        valueFun_foldl' f va vbs

valueFun_foldl1 :: InterpretM Value
valueFun_foldl1 = return $
  ValueFun $ \ (ValueFun f) -> return $
    ValueFun $ \ (ValueArray (vb:vbs)) ->
      valueFun_foldl' f vb vbs

valueFun_foldl' :: (Value -> InterpretM Value) -> Value -> [Value] -> InterpretM Value
valueFun_foldl' _ z [] = return z
valueFun_foldl' f z (x:xs) = do
  ValueFun fz <- f z
  fzx <- fz x
  valueFun_foldl' f fzx xs

valueFun_foldr :: InterpretM Value
valueFun_foldr = return $
  ValueFun $ \ (ValueFun f) -> return $
    ValueFun $ \ vb -> return $
      ValueFun $ \ (ValueArray vas) ->
        valueFun_foldr' f vb vas

valueFun_foldr1 :: InterpretM Value
valueFun_foldr1 = return $
  ValueFun $ \ (ValueFun f) -> return $
    ValueFun $ \ (ValueArray (va:vas)) ->
      valueFun_foldr' f va vas

valueFun_foldr' :: (Value -> InterpretM Value) -> Value -> [Value] -> InterpretM Value
valueFun_foldr' _ z [] = return z
valueFun_foldr' f z (x:xs) = do
  z' <- valueFun_foldr' f z xs
  ValueFun fx <- f x
  fx z'

valueFun_unroll :: InterpretM Value
valueFun_unroll = return $
  ValueFun $ \ (ValueFun f) -> return $
    ValueFun $ \ (ValueDFReal dfn) -> return $
      ValueFun $ \ z ->
        case dfn of
          DFRealLiteral _ i -> valueFun_unroll' f (floor i) z
          _ -> throwError $ "in function 'unroll', i must be statically determinable"

valueFun_unroll' :: (Value -> InterpretM Value) -> Int -> Value -> InterpretM Value
valueFun_unroll' _ 0 z = return z
valueFun_unroll' f i z = do
  fz <- f z
  valueFun_unroll' f (i-1) fz

valueFun_zipWith :: InterpretM Value
valueFun_zipWith = return $
  ValueFun $ \ (ValueFun f) -> return $
    ValueFun $ \ (ValueArray as) -> return $
      ValueFun $ \ (ValueArray bs) -> do
        vs <- valueFun_zipWith' f as bs
        return $ ValueArray vs

valueFun_zipWith' :: (Value -> InterpretM Value) -> [Value] -> [Value] -> InterpretM [Value]
valueFun_zipWith' z (a:as) (b:bs) = do
  ValueFun za <- z a
  zab <- za b
  zabs <- valueFun_zipWith' z as bs
  return $ zab:zabs
valueFun_zipWith' _ _ _ = return []

valueFun_zipWith3 :: InterpretM Value
valueFun_zipWith3 = return $
  ValueFun $ \ (ValueFun f) -> return $
    ValueFun $ \ (ValueArray as) -> return $
      ValueFun $ \ (ValueArray bs) -> return $
        ValueFun $ \ (ValueArray cs) -> do
          vs <- valueFun_zipWith3' f as bs cs
          return $ ValueArray vs

valueFun_zipWith3' :: (Value -> InterpretM Value) -> [Value] -> [Value] -> [Value] -> InterpretM [Value]
valueFun_zipWith3' z (a:as) (b:bs) (c:cs) = do
  ValueFun za <- z a
  ValueFun zab <- za b
  zabc <- zab c
  zabcs <- valueFun_zipWith3' z as bs cs
  return $ zabc:zabcs
valueFun_zipWith3' _ _ _ _= return []


-- Texture functions.

valueSample1D :: InterpretM Value
valueSample1D = return $
  ValueFun $ \ (ValueTexture1D i) -> return $
    ValueFun $ \ (ValueArray [ValueDFReal x]) -> do
    n <- freshNode
    nr <- freshNode
    ng <- freshNode
    nb <- freshNode
    na <- freshNode
    let submit = DFSample1D n i x
    return $ ValueArray [
      ValueDFReal $ DFRealGetTexR nr submit,
      ValueDFReal $ DFRealGetTexG ng submit,
      ValueDFReal $ DFRealGetTexB nb submit,
      ValueDFReal $ DFRealGetTexA na submit
      ]

valueSample2D :: InterpretM Value
valueSample2D = return $
  ValueFun $ \ (ValueTexture2D i) -> return $
    ValueFun $ \ (ValueArray [ValueDFReal x, ValueDFReal y]) -> do
    n <- freshNode
    nr <- freshNode
    ng <- freshNode
    nb <- freshNode
    na <- freshNode
    let submit = DFSample2D n i x y
    return $ ValueArray [
      ValueDFReal $ DFRealGetTexR nr submit,
      ValueDFReal $ DFRealGetTexG ng submit,
      ValueDFReal $ DFRealGetTexB nb submit,
      ValueDFReal $ DFRealGetTexA na submit
      ]

valueSample3D :: InterpretM Value
valueSample3D = return $
  ValueFun $ \ (ValueTexture3D i) -> return $
    ValueFun $ \ (ValueArray [ValueDFReal x, ValueDFReal y, ValueDFReal z]) -> do
    n <- freshNode
    nr <- freshNode
    ng <- freshNode
    nb <- freshNode
    na <- freshNode
    let submit = DFSample3D n i x y z
    return $ ValueArray [
      ValueDFReal $ DFRealGetTexR nr submit,
      ValueDFReal $ DFRealGetTexG ng submit,
      ValueDFReal $ DFRealGetTexB nb submit,
      ValueDFReal $ DFRealGetTexA na submit
      ]

valueSampleCube :: InterpretM Value
valueSampleCube = return $
  ValueFun $ \ (ValueTextureCube i) -> return $
    ValueFun $ \ (ValueArray [ValueDFReal x, ValueDFReal y, ValueDFReal z]) -> do
    n <- freshNode
    nr <- freshNode
    ng <- freshNode
    nb <- freshNode
    na <- freshNode
    let submit = DFSampleCube n i x y z
    return $ ValueArray [
      ValueDFReal $ DFRealGetTexR nr submit,
      ValueDFReal $ DFRealGetTexG ng submit,
      ValueDFReal $ DFRealGetTexB nb submit,
      ValueDFReal $ DFRealGetTexA na submit
      ]


-- Subscript.

valueSubscript :: InterpretM Value
valueSubscript = return $
  ValueFun $ \ (ValueArray vs) -> return $
    ValueFun $ \ (ValueDFReal sub) -> do
      let len = length vs
      case sub of
        DFRealLiteral _ d -> do
          let idx = floor d
          if 0 <= idx && idx < len
            then return $ vs!!idx
            else throwError $ "array index (" ++ show idx ++ ") out of bounds"
        _ -> throwError $ "array index is not statically determinable" -- todo: support dynamic indexing


-- Equality and inequality functions.

valueEqual :: InterpretM Value
valueEqual = return $
  ValueFun $ \ v1 -> return $
    ValueFun $ \ v2 -> valueEqual' v1 v2

valueEqual' :: Value -> Value -> InterpretM Value
valueEqual' (ValueUnit) (ValueUnit) = do
  n <- freshNode
  return $ ValueDFBool $ DFBoolLiteral n True
valueEqual' (ValueDFReal df1) (ValueDFReal df2) = liftRRB' (==) DFBoolEqualReal df1 df2
valueEqual' (ValueDFBool df1) (ValueDFBool df2) = liftBBB' (==) DFBoolEqualBool df1 df2
valueEqual' (ValueTexture1D i) (ValueTexture1D i') = do
  n <- freshNode
  return $ ValueDFBool $ DFBoolLiteral n $ i == i'
valueEqual' (ValueTexture2D i) (ValueTexture2D i') = do
  n <- freshNode
  return $ ValueDFBool $ DFBoolLiteral n $ i == i'
valueEqual' (ValueTexture3D i) (ValueTexture3D i') = do
  n <- freshNode
  return $ ValueDFBool $ DFBoolLiteral n $ i == i'
valueEqual' (ValueTextureCube i) (ValueTextureCube i') = do
  n <- freshNode
  return $ ValueDFBool $ DFBoolLiteral n $ i == i'
valueEqual' (ValueArray vs1) (ValueArray vs2) = do
  vs <- zipWithM valueEqual' vs1 vs2
  let (dfb:dfbs) = map unValueDFBool vs
  dfb' <- foldM (\x y -> do v <- liftBBB' (&&) DFBoolAnd x y; return $ unValueDFBool v) dfb dfbs
  return $ ValueDFBool dfb'
valueEqual' (ValueTuple vs1) (ValueTuple vs2) = do
  vs <- zipWithM valueEqual' vs1 vs2
  let (dfb:dfbs) = map unValueDFBool vs
  dfb' <- foldM (\x y -> do v <- liftBBB' (&&) DFBoolAnd x y; return $ unValueDFBool v) dfb dfbs
  return $ ValueDFBool dfb'
valueEqual' (ValueFun _) (ValueFun _) = throwError $ "equality is not defined on functions"
valueEqual' _ _ = undefined

valueNotEqual :: InterpretM Value
valueNotEqual = return $
  ValueFun $ \ v1 -> return $
    ValueFun $ \ v2 -> valueNotEqual' v1 v2

valueNotEqual' :: Value -> Value -> InterpretM Value
valueNotEqual' (ValueUnit) (ValueUnit) = do
  n <- freshNode
  return $ ValueDFBool $ DFBoolLiteral n False
valueNotEqual' (ValueDFReal df1) (ValueDFReal df2) = liftRRB' (/=) DFBoolNotEqualReal df1 df2
valueNotEqual' (ValueDFBool df1) (ValueDFBool df2) = liftBBB' (/=) DFBoolNotEqualBool df1 df2
valueNotEqual' (ValueTexture1D i) (ValueTexture1D i') = do
  n <- freshNode
  return $ ValueDFBool $ DFBoolLiteral n $ i /= i'
valueNotEqual' (ValueTexture2D i) (ValueTexture2D i') = do
  n <- freshNode
  return $ ValueDFBool $ DFBoolLiteral n $ i /= i'
valueNotEqual' (ValueTexture3D i) (ValueTexture3D i') = do
  n <- freshNode
  return $ ValueDFBool $ DFBoolLiteral n $ i /= i'
valueNotEqual' (ValueTextureCube i) (ValueTextureCube i') = do
  n <- freshNode
  return $ ValueDFBool $ DFBoolLiteral n $ i /= i'
valueNotEqual' (ValueArray vs1) (ValueArray vs2) = do
  vs <- zipWithM valueEqual' vs1 vs2
  let (dfb:dfbs) = map unValueDFBool vs
  dfb' <- foldM (\x y -> do v <- liftBBB' (||) DFBoolOr x y; return $ unValueDFBool v) dfb dfbs
  return $ ValueDFBool dfb'
valueNotEqual' (ValueTuple vs1) (ValueTuple vs2) = do
  vs <- zipWithM valueEqual' vs1 vs2
  let (dfb:dfbs) = map unValueDFBool vs
  dfb' <- foldM (\x y -> do v <- liftBBB' (||) DFBoolOr x y; return $ unValueDFBool v) dfb dfbs
  return $ ValueDFBool dfb'
valueNotEqual' (ValueFun _) (ValueFun _) = throwError $ "inequality is not defined on functions"
valueNotEqual' _ _ = undefined


-- Transpose function.
valueTranspose :: InterpretM Value
valueTranspose = return $
  ValueFun $ \ (ValueArray outer) -> return $
    ValueArray $ map ValueArray $ List.transpose $ map (\(ValueArray inner) -> inner) outer


-- (fixity, identifier, type scheme, desc, args different to GLSL?, arg list, definition)
libraryBase :: [(Fixity, String, String, String, Bool, [String], InterpretM Value)]
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
  (Prefix, "pi", "Real", "pi", False, [], do n <- freshNode; return $ ValueDFReal $ DFRealLiteral n pi),
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
  (Prefix, "floor", "Real -> Real", "round to negative infinity", False, ["x"], liftRR ((fromIntegral :: Integer -> Double) . floor) DFRealFloor),
  (Prefix, "ceiling", "Real -> Real", "round to positive infinity", False, ["x"], liftRR ((fromIntegral :: Integer -> Double) . ceiling) DFRealCeiling),
  (Prefix, "round", "Real -> Real", "round to nearest integer", False, ["x"], liftRR ((fromIntegral :: Integer -> Double) . round) DFRealRound),
  (Prefix, "truncate", "Real -> Real", "round to zero", False, ["x"], liftRR ((fromIntegral :: Integer -> Double) . truncate) DFRealTruncate),
  (Prefix, "fract", "Real -> Real", "fractional part", False, ["x"], liftRR (snd . (properFraction :: Double -> (Integer, Double))) DFRealFract),
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
  (Prefix, "pad", "pads fourth component with 1.0", False, "\\[x1, x2, x3]. [x1, x2, x3, 1.0]"),
  (Prefix, "strip", "strips fourth component", False, "\\[x1, x2, x3, x4]. [x1, x2, x3]")
  ]