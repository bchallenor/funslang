module Library(library, docLibrary) where

import qualified Data.ByteString.Lazy.Char8 as ByteString
import qualified Data.List as List
import qualified Data.Map as Map
import Control.Monad.Error

import Typing
import Parser
import Representation
import Interpreter
import Pretty


-- A tuple of:
-- - a map from library identifiers to type schemes;
-- - a map from library identifiers to values;
-- - the subsequent list of fresh variable references, given that some were used
--   in constructing the type schemes of the library functions.
library :: (SchemeEnv, ValueEnv, ([TypeVarRef], [DimVarRef]))
library =
  let wrapmsg ident msg = "in constructing library function <" ++ ident ++ ">: " ++ msg in
  
  let { base = List.foldl' (
    \ (gamma, env, vrefs) (ident, tstr, _, _, _, v) ->
      case parseType vrefs (ByteString.pack tstr) of
        Right (t, vrefs') ->
          let sigma = Scheme (fvType t) t in
            (Map.insert ident sigma gamma, Map.insert ident v env, vrefs')
        Left msg -> error $ wrapmsg ident msg
  ) (Map.empty, Map.empty, initFreshVarRefs) libraryBase } in
  
  List.foldl' (
    \ (gamma, env, vrefs) (ident, _, _, estr) ->
      case parseExpr vrefs (ByteString.pack estr) of
        Right (e, vrefs') ->
          case inferExprType gamma e vrefs' of
            Right (t, vrefs'') ->
              let sigma = Scheme (fvType t) t in
                (Map.insert ident sigma gamma, Map.insert ident (interpretExpr env e) env, vrefs'')
            Left msg -> error $ wrapmsg ident msg
        Left msg -> error $ wrapmsg ident msg
  ) base libraryDerived


-- Documents the library in LaTeX format.
docLibrary :: String
docLibrary =
  unlines $
    ["\\subsection{Base Functions}"]
    ++
    (map (\(ident, _, desc, _, _, _) -> docIdent ident desc Nothing) libraryBase)
    ++
    ["\\subsection{Derived Functions}"]
    ++
    (map (\(ident, desc, _, src) -> docIdent ident desc (Just src)) libraryDerived)
  
docIdent :: String -> String -> Maybe String -> String
docIdent ident desc opt_src =
  let (gamma, _, _) = library in
    case Map.lookup ident gamma of
      Nothing -> error $ ident ++ " should have been added to library"
      Just (Scheme _ t) ->
        let header = latexify (ident ++ " :: " ++ prettyType t) in
          case opt_src of
            Just src -> latexunlines [header, desc, latexify src] ++ "\n"
            Nothing -> latexunlines [header, desc] ++ "\n"

latexify :: String -> String
latexify s = latexunlines $ map (\l -> "\\verb\"" ++ l ++ "\"") $ lines s

latexunlines :: [String] -> String
latexunlines = concat . List.intersperse "\\newline\n"


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
liftRR sop dop = return $
  ValueFun $ \ (ValueDFReal df) -> do
    n <- freshNode
    case df of
      DFRealLiteral _ l -> return $ ValueDFReal $ DFRealLiteral n $ sop l
      _ -> return $ ValueDFReal $ dop n df

liftBB :: (Bool -> Bool) -> (Int -> DFBool -> DFBool) -> InterpretM Value
liftBB sop dop = return $
  ValueFun $ \ (ValueDFBool df) -> do
    n <- freshNode
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

valueFun_sample1D :: InterpretM Value
valueFun_sample1D = return $
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

valueFun_sample2D :: InterpretM Value
valueFun_sample2D = return $
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

valueFun_sample3D :: InterpretM Value
valueFun_sample3D = return $
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

valueFun_sampleCube :: InterpretM Value
valueFun_sampleCube = return $
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

valueFun_OpSubscript :: InterpretM Value
valueFun_OpSubscript = return $
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

valueFun_OpEqual :: InterpretM Value
valueFun_OpEqual = return $
  ValueFun $ \ v1 -> return $
    ValueFun $ \ v2 -> valueFun_OpEqual' v1 v2

valueFun_OpEqual' :: Value -> Value -> InterpretM Value
valueFun_OpEqual' (ValueUnit) (ValueUnit) = do
  n <- freshNode
  return $ ValueDFBool $ DFBoolLiteral n True
valueFun_OpEqual' (ValueDFReal df1) (ValueDFReal df2) = liftRRB' (==) DFBoolEqualReal df1 df2
valueFun_OpEqual' (ValueDFBool df1) (ValueDFBool df2) = liftBBB' (==) DFBoolEqualBool df1 df2
valueFun_OpEqual' (ValueTexture1D i) (ValueTexture1D i') = do
  n <- freshNode
  return $ ValueDFBool $ DFBoolLiteral n $ i == i'
valueFun_OpEqual' (ValueTexture2D i) (ValueTexture2D i') = do
  n <- freshNode
  return $ ValueDFBool $ DFBoolLiteral n $ i == i'
valueFun_OpEqual' (ValueTexture3D i) (ValueTexture3D i') = do
  n <- freshNode
  return $ ValueDFBool $ DFBoolLiteral n $ i == i'
valueFun_OpEqual' (ValueTextureCube i) (ValueTextureCube i') = do
  n <- freshNode
  return $ ValueDFBool $ DFBoolLiteral n $ i == i'
valueFun_OpEqual' (ValueArray vs1) (ValueArray vs2) = do
  vs <- zipWithM valueFun_OpEqual' vs1 vs2
  let (dfb:dfbs) = map unValueDFBool vs
  dfb' <- foldM (\x y -> do v <- liftBBB' (&&) DFBoolAnd x y; return $ unValueDFBool v) dfb dfbs
  return $ ValueDFBool dfb'
valueFun_OpEqual' (ValueTuple vs1) (ValueTuple vs2) = do
  vs <- zipWithM valueFun_OpEqual' vs1 vs2
  let (dfb:dfbs) = map unValueDFBool vs
  dfb' <- foldM (\x y -> do v <- liftBBB' (&&) DFBoolAnd x y; return $ unValueDFBool v) dfb dfbs
  return $ ValueDFBool dfb'
valueFun_OpEqual' (ValueFun _) (ValueFun _) = throwError $ "equality is not defined on functions"
valueFun_OpEqual' _ _ = undefined

valueFun_OpNotEqual :: InterpretM Value
valueFun_OpNotEqual = return $
  ValueFun $ \ v1 -> return $
    ValueFun $ \ v2 -> valueFun_OpNotEqual' v1 v2

valueFun_OpNotEqual' :: Value -> Value -> InterpretM Value
valueFun_OpNotEqual' (ValueUnit) (ValueUnit) = do
  n <- freshNode
  return $ ValueDFBool $ DFBoolLiteral n False
valueFun_OpNotEqual' (ValueDFReal df1) (ValueDFReal df2) = liftRRB' (/=) DFBoolNotEqualReal df1 df2
valueFun_OpNotEqual' (ValueDFBool df1) (ValueDFBool df2) = liftBBB' (/=) DFBoolNotEqualBool df1 df2
valueFun_OpNotEqual' (ValueTexture1D i) (ValueTexture1D i') = do
  n <- freshNode
  return $ ValueDFBool $ DFBoolLiteral n $ i /= i'
valueFun_OpNotEqual' (ValueTexture2D i) (ValueTexture2D i') = do
  n <- freshNode
  return $ ValueDFBool $ DFBoolLiteral n $ i /= i'
valueFun_OpNotEqual' (ValueTexture3D i) (ValueTexture3D i') = do
  n <- freshNode
  return $ ValueDFBool $ DFBoolLiteral n $ i /= i'
valueFun_OpNotEqual' (ValueTextureCube i) (ValueTextureCube i') = do
  n <- freshNode
  return $ ValueDFBool $ DFBoolLiteral n $ i /= i'
valueFun_OpNotEqual' (ValueArray vs1) (ValueArray vs2) = do
  vs <- zipWithM valueFun_OpEqual' vs1 vs2
  let (dfb:dfbs) = map unValueDFBool vs
  dfb' <- foldM (\x y -> do v <- liftBBB' (||) DFBoolOr x y; return $ unValueDFBool v) dfb dfbs
  return $ ValueDFBool dfb'
valueFun_OpNotEqual' (ValueTuple vs1) (ValueTuple vs2) = do
  vs <- zipWithM valueFun_OpEqual' vs1 vs2
  let (dfb:dfbs) = map unValueDFBool vs
  dfb' <- foldM (\x y -> do v <- liftBBB' (||) DFBoolOr x y; return $ unValueDFBool v) dfb dfbs
  return $ ValueDFBool dfb'
valueFun_OpNotEqual' (ValueFun _) (ValueFun _) = throwError $ "inequality is not defined on functions"
valueFun_OpNotEqual' _ _ = undefined


-- Transpose function.
valueFun_tx :: InterpretM Value
valueFun_tx = return $
  ValueFun $ \ (ValueArray outer) -> return $
    ValueArray $ map ValueArray $ List.transpose $ map (\(ValueArray inner) -> inner) outer


-- (identifier, type, desc, args different to GLSL?, arg list, value)
libraryBase :: [(String, String, String, Bool, [String], InterpretM Value)]
libraryBase = [
  (show OpScalarNeg, "Real -> Real", "scalar negate (as desugared from `-')", False, ["x"], liftRR (negate) DFRealNeg),
  ("not", "Bool -> Bool", "logical not", False, ["x"], liftBB (not) DFBoolNot),
  (show OpSubscript, "a n -> Real -> a", "subscript", False, ["as", "n"], valueFun_OpSubscript),
  (show OpScalarAdd, "Real -> Real -> Real", "scalar add", False, ["x", "y"], liftRRR (+) DFRealAdd),
  (show OpScalarSub, "Real -> Real -> Real", "scalar sub", False, ["x", "y"], liftRRR (-) DFRealSub),
  (show OpScalarMul, "Real -> Real -> Real", "scalar mul", False, ["x", "y"], liftRRR (*) DFRealMul),
  (show OpScalarDiv, "Real -> Real -> Real", "scalar div", False, ["x", "y"], liftRRR (/) DFRealDiv),
  (show OpLessThan, "Real -> Real -> Bool", "less than", False, ["x", "y"], liftRRB (<) DFBoolLessThan),
  (show OpLessThanEqual, "Real -> Real -> Bool", "less than or equal", False, ["x", "y"], liftRRB (<=) DFBoolLessThanEqual),
  (show OpGreaterThan, "Real -> Real -> Bool", "greater than", False, ["x", "y"], liftRRB (>) DFBoolGreaterThan),
  (show OpGreaterThanEqual, "Real -> Real -> Bool", "greater than or equal", False, ["x", "y"], liftRRB (>=) DFBoolGreaterThanEqual),
  (show OpEqual, "a -> a -> Bool", "equality (not defined on functions)", False, ["x", "y"], valueFun_OpEqual),
  (show OpNotEqual, "a -> a -> Bool", "inequality (not defined on functions)", False, ["x", "y"], valueFun_OpNotEqual),
  (show OpAnd, "Bool -> Bool -> Bool", "logical and", False, ["x", "y"], liftBBB (&&) DFBoolAnd),
  (show OpOr, "Bool -> Bool -> Bool", "logical or", False, ["x", "y"], liftBBB (||) DFBoolOr),
  ("tx", "a p q -> a q p", "transpose", False, ["x"], valueFun_tx),
  ("map", "(a -> b) -> a n -> b n", "map function onto array", False, ["f", "as"], valueFun_map),
  ("foldl", "(a -> a -> b) -> a -> b n -> a", "left fold", False, ["f", "z", "bs"], valueFun_foldl),
  ("foldl1", "(a -> a -> a) -> a n -> a", "left fold without initial accumulator", False, ["f", "as"], valueFun_foldl1),
  ("foldr", "(a -> b -> b) -> b -> a n -> b", "right fold", False, ["f", "z", "as"], valueFun_foldr),
  ("foldr1", "(a -> a -> a) -> a n -> a", "right fold without initial accumulator", False, ["f", "as"], valueFun_foldr1),
  ("unroll", "(a -> a) -> Real -> a -> a", "apply f n times to z (n must be statically determinable)", False, ["f", "n", "z"], valueFun_unroll),
  ("zipWith", "(a -> b -> c) -> a n -> b n -> c n", "general zip over 2 arrays", False, ["f", "as", "bs"], valueFun_zipWith),
  ("zipWith3", "(a -> b -> c -> d) -> a n -> b n -> c n -> d n", "general zip over 3 arrays", False, ["f", "as", "bs", "cs"], valueFun_zipWith3),
  ("pi", "Real", "pi", False, [], do n <- freshNode; return $ ValueDFReal $ DFRealLiteral n pi),
  ("sin", "Real -> Real", "sine (radians)", False, ["a"], liftRR sin DFRealSin),
  ("cos", "Real -> Real", "cosine (radians)", False, ["a"], liftRR cos DFRealCos),
  ("tan", "Real -> Real", "tangent (radians)", False, ["a"], liftRR tan DFRealTan),
  ("asin", "Real -> Real", "arcsine (radians)", False, ["x"], liftRR asin DFRealASin),
  ("acos", "Real -> Real", "arccosine (radians)", False, ["x"], liftRR acos DFRealACos),
  ("atan", "Real -> Real -> Real", "arctangent (radians)", False, ["x", "y"], liftRR atan DFRealATan),
  ("pow", "Real -> Real -> Real", "power", False, ["x", "y"], liftRRR (**) DFRealPow),
  ("exp", "Real -> Real", "power (base e)", False, ["x"], liftRR exp DFRealExp),
  ("exp2", "Real -> Real", "power (base 2)", False, ["x"], liftRR (2**) DFRealExp2),
  ("log", "Real -> Real", "logarithm (base e)", False, ["x"], liftRR log DFRealLog),
  ("log2", "Real -> Real", "logarithm (base 2)", False, ["x"], liftRR (logBase 2) DFRealLog2),
  ("rsqrt", "Real -> Real", "reciprocal square root", False, ["x"], liftRR (\x -> 1 / sqrt x) DFRealRsq),
  ("abs", "Real -> Real", "absolute value", False, ["x"], liftRR abs DFRealAbs),
  ("floor", "Real -> Real", "round to negative infinity", False, ["x"], liftRR ((fromIntegral :: Integer -> Double) . floor) DFRealFloor),
  ("ceiling", "Real -> Real", "round to positive infinity", False, ["x"], liftRR ((fromIntegral :: Integer -> Double) . ceiling) DFRealCeiling),
  ("round", "Real -> Real", "round to nearest integer", False, ["x"], liftRR ((fromIntegral :: Integer -> Double) . round) DFRealRound),
  ("truncate", "Real -> Real", "round to zero", False, ["x"], liftRR ((fromIntegral :: Integer -> Double) . truncate) DFRealTruncate),
  ("fract", "Real -> Real", "fractional part", False, ["x"], liftRR (snd . (properFraction :: Double -> (Integer, Double))) DFRealFract),
  ("min", "Real -> Real -> Real", "minimum", False, ["x", "y"], liftRRR min DFRealMin),
  ("max", "Real -> Real -> Real", "maximum", False, ["x", "y"], liftRRR max DFRealMax),
  ("sample1D", "Texture1D -> Real 1 -> Real 4", "sample 1D texture", False, ["tex", "coord"], valueFun_sample1D),
  ("sample2D", "Texture2D -> Real 2 -> Real 4", "sample 2D texture", False, ["tex", "coord"], valueFun_sample2D),
  ("sample3D", "Texture3D -> Real 3 -> Real 4", "sample 3D texture", False, ["tex", "coord"], valueFun_sample3D),
  ("sampleCube", "TextureCube -> Real 3 -> Real 4", "sample cubic texture", False, ["tex", "coord"], valueFun_sampleCube)
  ]

-- (identifier, desc, args different to GLSL?, funslang source)
libraryDerived :: [(String, String, Bool, String)]
libraryDerived = [
  (show OpApply, "function application operator", False, "\\f x. f x"),
  (show OpVectorNeg, "vector negate (component-wise) (as desugared from `--')", False, "map negate"),
  (show OpSwizzle, "swizzle", False, "\\as ns. map (as!) ns"),
  (show OpVectorAdd, "vector add (component-wise)", False, "zipWith (+)"),
  (show OpVectorSub, "vector sub (component-wise)", False, "zipWith (-)"),
  (show OpVectorMul, "vector mul (component-wise)", False, "zipWith (*)"),
  (show OpVectorDiv, "vector div (component-wise)", False, "zipWith (/)"),
  (show OpVectorScalarMul, "vector-scalar mul", False, "\\xs y. map (*y) xs"),
  (show OpVectorScalarDiv, "vector-scalar div", False, "\\xs y. map (/y) xs"),
  ("sum", "sum of components", False, "foldl1 (+)"),
  ("product", "product of components", False, "foldl1 (*)"),
  ("any", "logical or of components", False, "foldl1 (||)"),
  ("all", "logical and of components", False, "foldl1 (&&)"),
  ("sqrt", "square root", False, "\\x. 1 / rsqrt x"),
  ("mod", "modulus", False, "\\x y. x - y * floor (x/y)"),
  ("dot", "dot product", False, "\\x y. sum $ x ** y"),
  ("cross", "cross product", False, "\\[x1, x2, x3] [y1, y2, y3].\n[x2 * y3 - x3 * y2, x3 * y1 - x1 * y3, x1 * y2 - x2 * y1]"),
  ("length", "vector length (Pythagorean)", False, "\\x. sqrt $ dot x x"),
  ("normalize", "normalize", False, "\\x. x **. (rsqrt $ dot x x)"),
  (show OpMatrixVectorLinearMul, "matrix-vector linear algebraic mul", False, "\\m v. map (dot v) m"),
  (show OpVectorMatrixLinearMul, "vector-matrix linear algebraic mul", False, "\\v m. map (dot v) (tx m)"),
  (show OpMatrixMatrixLinearMul, "matrix-matrix linear algebraic mul", False, "\\ma mb. tx $ map (ma #.) (tx mb)"),
  ("clamp", "clamp value to given range", True, "\\low high x. min (max x low) high"),
  ("step", "unit step", False, "\\edge x. if x < edge then 0 else 1"),
  ("mix", "linear interpolation", True, "\\a x y. x * (1 - a) + y * a"),
  ("smoothstep", "hermite interpolation", False, "\\edge0 edge1 x.\nlet t = clamp 0 1 ((x - edge0) / (edge1 - edge0)) in\n  t * t * (3 - 2 * t)"),
  ("faceforward", "returns V facing forward", False, "\\V I N. if dot N I < 0 then V else --V"),
  ("reflect", "reflect I given N (normalized)", False, "\\I N. I -- N **. (2 * dot N I)"),
  ("refract", "refract I given N (normalized) and index eta", False, "\\I N eta.\nlet d = dot N I in\nlet eta2 = eta * eta in\nlet k = 1 - eta2 + eta2 * d * d in\n  if k < 0\n    then map (\\_. 0) N\n    else I **. eta -- N **. (eta * d + sqrt k)"),
  ("pad", "pads fourth component with 1.0", False, "\\[x1, x2, x3]. [x1, x2, x3, 1.0]"),
  ("strip", "strips fourth component", False, "\\[x1, x2, x3, _]. [x1, x2, x3]")
  ]
