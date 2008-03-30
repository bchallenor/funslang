module Library(library, docLibrary, printLibrarySchemes) where

import qualified Data.ByteString.Lazy.Char8 as ByteString
import qualified Data.List as List
import qualified Data.Map as Map
import Control.Monad.Error

import Typing
import Parser
import Representation
import Interpreter
import Pretty
import CompileError

library :: Library
library =
  let wrapmsg ident msg = "in constructing library function <" ++ ident ++ ">: " ++ msg in
  
  let { base = List.foldl' (
    \ (gamma, env, vrefs) (ident, tstr, _, v) ->
      case parseType vrefs (ByteString.pack tstr) of
        Right (t, vrefs') ->
          -- There are no free type variables in the library so we don't have to worry about capture.
          let sigma = Scheme (fvType t) t in
            (Map.insert ident sigma gamma, Map.insert ident v env, vrefs')
        Left err -> error $ wrapmsg ident (getErrorString err)
  ) (Map.empty, Map.empty, initFreshVarRefs) libraryBase } in
  
  List.foldl' (
    \ (gamma, env, vrefs) (ident, _, estr) ->
      case parseExpr vrefs (ByteString.pack estr) of
        Right (e, vrefs') ->
          case inferExprType gamma e vrefs' of
            Right (t, vrefs'') ->
              -- There are no free type variables in the library so we don't have to worry about capture.
              let sigma = Scheme (fvType t) t in
                (Map.insert ident sigma gamma, Map.insert ident (interpretExpr env e) env, vrefs'')
            Left err -> error $ wrapmsg ident (getErrorString err)
        Left err -> error $ wrapmsg ident (getErrorString err)
  ) base libraryDerived


-- Documents the library in LaTeX format.
docLibrary :: String
docLibrary =
  unlines $
    ["\\subsection{Base Functions}"]
    ++
    (map (\(ident, _, desc, _) -> docIdent ident desc Nothing) libraryBase)
    ++
    ["\\subsection{Derived Functions}"]
    ++
    (map (\(ident, desc, src) -> docIdent ident desc (Just src)) libraryDerived)
  
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


-- Prints the given Library's type schemes.
printLibrarySchemes :: Library -> IO ()
printLibrarySchemes (gamma, _, _) = do
  mapM_ (\(ident, scheme) -> putStrLn $ ident ++ ": " ++ prettySchemeDebug scheme) (Map.assocs gamma)


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
          _ -> throwError $ InterpreterError [] $ InterpreterErrorDynamicUnroll

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


-- Texture sampling function.

valueFun_sample :: InterpretM Value
valueFun_sample = return $
  ValueFun $ \ (ValueTex tk i) -> return $
    ValueFun $ \ (ValueArray coord_vs) -> do
    let coord_dfs = map unValueDFReal coord_vs
    n <- freshNode
    nr <- freshNode
    ng <- freshNode
    nb <- freshNode
    na <- freshNode
    let submit = DFSampleTex n tk i coord_dfs
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
            else throwError $ InterpreterError [] $ InterpreterErrorArrayIndexOutOfBounds idx
        _ -> throwError $ InterpreterError [] $ InterpreterErrorDynamicIndex


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
valueFun_OpEqual' (ValueTex _ i) (ValueTex _ i') = do
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
valueFun_OpEqual' (ValueFun _) (ValueFun _) = throwError $ InterpreterError [] $ InterpreterErrorFunctionEquality
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
valueFun_OpNotEqual' (ValueTex _ i) (ValueTex _ i') = do
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
valueFun_OpNotEqual' (ValueFun _) (ValueFun _) = throwError $ InterpreterError [] $ InterpreterErrorFunctionEquality
valueFun_OpNotEqual' _ _ = undefined


-- Transpose function.
valueFun_tx :: InterpretM Value
valueFun_tx = return $
  ValueFun $ \ (ValueArray outer) -> return $
    ValueArray $ map ValueArray $ List.transpose $ map (\(ValueArray inner) -> inner) outer


-- (identifier, type, description, value)
libraryBase :: [(String, String, String, InterpretM Value)]
libraryBase = [
  (show OpScalarNeg, "Real -> Real", "scalar negate (as desugared from `-')", liftRR (negate) DFRealNeg),
  ("not", "Bool -> Bool", "logical not", liftBB (not) DFBoolNot),
  (show OpSubscript, "a n -> Real -> a", "subscript", valueFun_OpSubscript),
  (show OpScalarAdd, "Real -> Real -> Real", "scalar add", liftRRR (+) DFRealAdd),
  (show OpScalarSub, "Real -> Real -> Real", "scalar sub", liftRRR (-) DFRealSub),
  (show OpScalarMul, "Real -> Real -> Real", "scalar mul", liftRRR (*) DFRealMul),
  (show OpScalarDiv, "Real -> Real -> Real", "scalar div", liftRRR (/) DFRealDiv),
  (show OpLessThan, "Real -> Real -> Bool", "less than", liftRRB (<) DFBoolLessThan),
  (show OpLessThanEqual, "Real -> Real -> Bool", "less than or equal", liftRRB (<=) DFBoolLessThanEqual),
  (show OpGreaterThan, "Real -> Real -> Bool", "greater than", liftRRB (>) DFBoolGreaterThan),
  (show OpGreaterThanEqual, "Real -> Real -> Bool", "greater than or equal", liftRRB (>=) DFBoolGreaterThanEqual),
  (show OpEqual, "a -> a -> Bool", "equality (not defined on functions)", valueFun_OpEqual),
  (show OpNotEqual, "a -> a -> Bool", "inequality (not defined on functions)", valueFun_OpNotEqual),
  (show OpAnd, "Bool -> Bool -> Bool", "logical and", liftBBB (&&) DFBoolAnd),
  (show OpOr, "Bool -> Bool -> Bool", "logical or", liftBBB (||) DFBoolOr),
  ("tx", "a p q -> a q p", "transpose", valueFun_tx),
  ("map", "(a -> b) -> a n -> b n", "map function onto array", valueFun_map),
  ("foldl", "(a -> a -> b) -> a -> b n -> a", "left fold", valueFun_foldl),
  ("foldl1", "(a -> a -> a) -> a n -> a", "left fold without initial accumulator", valueFun_foldl1),
  ("foldr", "(a -> b -> b) -> b -> a n -> b", "right fold", valueFun_foldr),
  ("foldr1", "(a -> a -> a) -> a n -> a", "right fold without initial accumulator", valueFun_foldr1),
  ("unroll", "(a -> a) -> Real -> a -> a", "apply f n times to z (support for dynamic n is not mandated)", valueFun_unroll),
  ("zipWith", "(a -> b -> c) -> a n -> b n -> c n", "general zip over 2 arrays", valueFun_zipWith),
  ("zipWith3", "(a -> b -> c -> d) -> a n -> b n -> c n -> d n", "general zip over 3 arrays", valueFun_zipWith3),
  ("sin", "Real -> Real", "sine (radians)", liftRR sin DFRealSin),
  ("cos", "Real -> Real", "cosine (radians)", liftRR cos DFRealCos),
  ("tan", "Real -> Real", "tangent (radians)", liftRR tan DFRealTan),
  ("asin", "Real -> Real", "arcsine (radians)", liftRR asin DFRealASin),
  ("acos", "Real -> Real", "arccosine (radians)", liftRR acos DFRealACos),
  ("atan", "Real -> Real -> Real", "arctangent (radians)", liftRR atan DFRealATan),
  ("pow", "Real -> Real -> Real", "power", liftRRR (**) DFRealPow),
  ("exp", "Real -> Real", "power (base e)", liftRR exp DFRealExp),
  ("exp2", "Real -> Real", "power (base 2)", liftRR (2**) DFRealExp2),
  ("log", "Real -> Real", "logarithm (base e)", liftRR log DFRealLog),
  ("log2", "Real -> Real", "logarithm (base 2)", liftRR (logBase 2) DFRealLog2),
  ("rsqrt", "Real -> Real", "reciprocal square root", liftRR (\x -> 1 / sqrt x) DFRealRsq),
  ("abs", "Real -> Real", "absolute value", liftRR abs DFRealAbs),
  ("floor", "Real -> Real", "round to negative infinity", liftRR ((fromIntegral :: Integer -> Double) . floor) DFRealFloor),
  ("ceiling", "Real -> Real", "round to positive infinity", liftRR ((fromIntegral :: Integer -> Double) . ceiling) DFRealCeiling),
  ("round", "Real -> Real", "round to nearest integer", liftRR ((fromIntegral :: Integer -> Double) . round) DFRealRound),
  ("truncate", "Real -> Real", "round to zero", liftRR ((fromIntegral :: Integer -> Double) . truncate) DFRealTruncate),
  ("fract", "Real -> Real", "fractional part", liftRR (snd . (properFraction :: Double -> (Integer, Double))) DFRealFract),
  ("min", "Real -> Real -> Real", "minimum", liftRRR min DFRealMin),
  ("max", "Real -> Real -> Real", "maximum", liftRRR max DFRealMax),
  ("sample1D", "Tex 1D -> Real 1 -> Real 4", "sample 1D texture", valueFun_sample),
  ("sample2D", "Tex 2D -> Real 2 -> Real 4", "sample 2D texture", valueFun_sample),
  ("sample3D", "Tex 3D -> Real 3 -> Real 4", "sample 3D texture", valueFun_sample),
  ("sampleCube", "Tex Cube -> Real 3 -> Real 4", "sample cubic texture", valueFun_sample)
  ]

-- (identifier, description, funslang source)
libraryDerived :: [(String, String, String)]
libraryDerived = [
  (show OpApply, "function application operator", "\\f x -> f x"),
  (show OpCompose, "function composition operator", "\\f g x -> f (g x)"),
  (show OpVectorNeg, "vector negate (component-wise) (as desugared from `--')", "map negate"),
  (show OpSwizzle, "swizzle", "\\as ns -> map (as!) ns"),
  (show OpVectorAdd, "vector add (component-wise)", "zipWith (+)"),
  (show OpVectorSub, "vector sub (component-wise)", "zipWith (-)"),
  (show OpVectorMul, "vector mul (component-wise)", "zipWith (*)"),
  (show OpVectorDiv, "vector div (component-wise)", "zipWith (/)"),
  (show OpVectorScalarMul, "vector-scalar mul", "\\xs y -> map (*y) xs"),
  (show OpVectorScalarDiv, "vector-scalar div", "\\xs y -> map (/y) xs"),
  ("pi", "pi", show (pi::Double)),
  ("sum", "sum of components", "foldl1 (+)"),
  ("product", "product of components", "foldl1 (*)"),
  ("any", "logical or of components", "foldl1 (||)"),
  ("all", "logical and of components", "foldl1 (&&)"),
  ("sqrt", "square root", "\\x -> 1 / rsqrt x"),
  ("mod", "modulus", "\\x y -> x - y * floor (x/y)"),
  ("dot", "dot product", "\\x y -> sum $ x ** y"),
  ("cross", "cross product", "\\[x1, x2, x3] [y1, y2, y3] ->\n[x2 * y3 - x3 * y2, x3 * y1 - x1 * y3, x1 * y2 - x2 * y1]"),
  ("length", "vector length (Pythagorean)", "\\x -> sqrt $ dot x x"),
  ("normalize", "normalize", "\\x -> x **. (rsqrt $ dot x x)"),
  (show OpMatrixVectorLinearMul, "matrix-vector linear algebraic mul", "\\m v -> map (dot v) m"),
  (show OpVectorMatrixLinearMul, "vector-matrix linear algebraic mul", "\\v m -> map (dot v) (tx m)"),
  (show OpMatrixMatrixLinearMul, "matrix-matrix linear algebraic mul", "\\ma mb -> tx $ map (ma #.) (tx mb)"),
  ("clamp", "clamp value to given range", "\\low high x -> min (max x low) high"), -- note arg order different to GLSL
  ("step", "unit step", "\\edge x -> if x < edge then 0 else 1"),
  ("mix", "linear interpolation", "\\a x y -> x * (1 - a) + y * a"), -- note arg order different to GLSL
  ("smoothstep", "hermite interpolation", "\\edge0 edge1 x ->\nlet t = clamp 0 1 ((x - edge0) / (edge1 - edge0)) in\n  t * t * (3 - 2 * t)"),
  ("faceforward", "returns v facing forward", "\\v i n -> if dot n i < 0 then v else --v"),
  ("reflect", "reflect i given n (normalized)", "\\i n -> i -- n **. (2 * dot n i)"),
  ("refract", "refract i given n (normalized) and index eta", "\\i n eta ->\nlet d = dot n i in\nlet eta2 = eta * eta in\nlet k = 1 - eta2 + eta2 * d * d in\n  if k < 0\n    then map (\\_ -> 0) n\n    else i **. eta -- n **. (eta * d + sqrt k)"),
  ("pad", "pads fourth component with 1.0", "\\[x1, x2, x3] -> [x1, x2, x3, 1.0]"),
  ("strip", "strips fourth component", "\\[x1, x2, x3, _] -> [x1, x2, x3]")
  ]
