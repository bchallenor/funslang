module Test(doTestGroups) where

import qualified Data.Map as Map
import qualified Data.ByteString.Lazy.Char8 as ByteString
import Control.Monad

import Representation
import Parser
import Typing
import Library
import Compiler
import CompileError


-- Runs all tests and dos the results.

doTestGroups :: IO Bool
doTestGroups = do
  (num_pass, num_fail, num_err) <- foldM doTestGroup (0,0,0) testgroups
  putStrLn "\n"
  putStrLn $ show num_pass ++ " tests passed, " ++ show num_fail ++ " tests failed and " ++ show num_err ++ " tests had unexpected errors."
  if num_fail == 0 && num_err == 0
    then return True
    else return False

doTestGroup :: (Int, Int, Int) -> TestGroup -> IO (Int, Int, Int)
doTestGroup acc tg = do
  putStrLn $ "Testing " ++ group_title tg ++ ":"
  foldM doTest acc (group_tests tg)

doTest :: (Int, Int, Int) -> Test -> IO (Int, Int, Int)
doTest (num_pass, num_fail, num_err) test = do
  putStr $ "  " ++ (name test) ++ "... "
  case runTest test of
    ResultPass -> do
      putStrLn $ "OK"
      return (num_pass+1, num_fail, num_err)
    ResultFail -> do
      putStrLn $ "FAIL"
      return (num_pass, num_fail+1, num_err)
    ResultUnexpectedError err -> do
      putStrLn $ "UNEXPECTED FAIL: " ++ getErrorString err
      return (num_pass, num_fail, num_err+1)


-- Test case types.

data TestGroup
  = TestGroup { group_title :: !String, group_tests :: ![Test] }

data Test
  = TestExprValue { name :: !String, expr :: !String, expect_value :: !String }
  | TestExprType { name :: !String, expr :: !String, expect_type :: !String }
  | TestExprCompileError { name :: !String, expr :: !String, expect_error :: !(CompileError -> Bool) }
  | TestShaderCompileError { name :: !String, vert :: !String, frag :: !String, expect_error :: !(CompileError -> Bool) }

data Result
  = ResultPass
  | ResultFail
  | ResultUnexpectedError !CompileError
  
  deriving Show


-- Expression test cases.

testgroups :: [TestGroup]
testgroups = [
  TestGroup { group_title = "typing rules", group_tests = [
    TestExprType { name = "unit", expr = "()", expect_type = "()" },
    TestExprType { name = "real 1", expr = "1", expect_type = "Real" },
    TestExprType { name = "real 0", expr = "0", expect_type = "Real" },
    TestExprType { name = "real -1", expr = "-1", expect_type = "Real" },
    TestExprType { name = "bool T", expr = "True", expect_type = "Bool" },
    TestExprType { name = "bool F", expr = "False", expect_type = "Bool" },
    TestExprType { name = "var", expr = "pi", expect_type = "Real" },
    TestExprType { name = "app fn", expr = "sin", expect_type = "Real -> Real" },
    TestExprType { name = "app res", expr = "sin 0", expect_type = "Real" },
    TestExprType { name = "array", expr = "[1, 0, -1]", expect_type = "Real 3" },
    TestExprType { name = "tuple", expr = "(True, 1)", expect_type = "(Bool, Real)" },
    TestExprType { name = "if", expr = "if False then 0 else 1", expect_type = "Real" },
    TestExprType { name = "let", expr = "let f (x, [y, z]) = 0 in f", expect_type = "(a, b 2) -> Real" },
    TestExprType { name = "lambda", expr = "\\x -> x", expect_type = "a -> a" }
    ] },
    
  TestGroup { group_title = "errors", group_tests = [
    TestExprCompileError { name = "LexerErrorNoLex", expr = "\0", expect_error = \ e -> case e of LexerError _ (LexerErrorNoLex '\0') -> True; _ -> False },
    TestExprCompileError { name = "LexerErrorIdentBeginsUpper", expr = "N", expect_error = \ e -> case e of LexerError _ (LexerErrorIdentBeginsUpper "N") -> True; _ -> False },
    TestExprCompileError { name = "ParserErrorNoParse", expr = "", expect_error = \ e -> case e of ParserError _ (ParserErrorNoParse _) -> True; _ -> False },
    TestExprCompileError { name = "ParserErrorBadFixedDim", expr = "\\(x :: Real 0) -> x", expect_error = \ e -> case e of ParserError _ (ParserErrorBadFixedDim 0) -> True; _ -> False },
    TestExprCompileError { name = "TypeErrorOccursCheck", expr = "\\x -> x x", expect_error = \ e -> case e of TypeError _ (TypeErrorOccursCheck _ _) -> True; _ -> False },
    TestExprCompileError { name = "TypeErrorCouldNotUnify", expr = "[True, 1]", expect_error = \ e -> case e of TypeError _ (TypeErrorCouldNotUnify _ _) -> True; _ -> False },
    TestExprCompileError { name = "TypeErrorUnboundVariable", expr = "x", expect_error = \ e -> case e of TypeError _ (TypeErrorUnboundVariable _) -> True; _ -> False },
    TestExprCompileError { name = "TypeErrorDuplicateIdentsInPattern-array", expr = "\\[x, x] -> x", expect_error = \ e -> case e of TypeError _ (TypeErrorDuplicateIdentsInPattern _) -> True; _ -> False },
    TestExprCompileError { name = "TypeErrorDuplicateIdentsInPattern-tuple", expr = "\\(x, x) -> x", expect_error = \ e -> case e of TypeError _ (TypeErrorDuplicateIdentsInPattern _) -> True; _ -> False },
    TestShaderCompileError { name = "ShaderErrorBadShaderType", vert = "1", frag = "", expect_error = \ e -> case e of ShaderError _ (ShaderErrorBadShaderType _) -> True; _ -> False },
    TestShaderCompileError { name = "ShaderErrorBadUniformType-tex", vert = "\\(u :: Tex 1D)()() -> ([0,0,0,0], ())", frag = "\\()()() -> [0,0,0,0]", expect_error = \ e -> case e of ShaderError ShaderKindVertex (ShaderErrorBadUniformType _) -> True; _ -> False },
    TestShaderCompileError { name = "ShaderErrorBadUniformType-fn", vert = "\\(u :: Real -> Real)()() -> ([0,0,0,0], ())", frag = "\\()()() -> [0,0,0,0]", expect_error = \ e -> case e of ShaderError ShaderKindVertex (ShaderErrorBadUniformType _) -> True; _ -> False },
    TestShaderCompileError { name = "ShaderErrorBadTextureType-real", vert = "\\()(t :: Real)() -> ([0,0,0,0], ())", frag = "\\()()() -> [0,0,0,0]", expect_error = \ e -> case e of ShaderError ShaderKindVertex (ShaderErrorBadTextureType _) -> True; _ -> False },
    TestShaderCompileError { name = "ShaderErrorBadTextureType-fn", vert = "\\()(t :: Real -> Real)() -> ([0,0,0,0], ())", frag = "\\()()() -> [0,0,0,0]", expect_error = \ e -> case e of ShaderError ShaderKindVertex (ShaderErrorBadTextureType _) -> True; _ -> False },
    TestShaderCompileError { name = "ShaderErrorBadVaryingType-tex", vert = "\\()()(v :: Tex 1D) -> ([0,0,0,0], ())", frag = "\\()()() -> [0,0,0,0]", expect_error = \ e -> case e of ShaderError ShaderKindVertex (ShaderErrorBadVaryingType _) -> True; _ -> False },
    TestShaderCompileError { name = "ShaderErrorBadVaryingType-fn", vert = "\\()()(v :: Real -> Real) -> ([0,0,0,0], ())", frag = "\\()()() -> [0,0,0,0]", expect_error = \ e -> case e of ShaderError ShaderKindVertex (ShaderErrorBadVaryingType _) -> True; _ -> False },
    TestShaderCompileError { name = "ShaderErrorBadOutputType", vert = "\\()(t :: Tex 1D)() -> ([0,0,0,0], t)", frag = "\\()()(t :: Tex 1D) -> [0,0,0,0]", expect_error = \ e -> case e of ShaderError ShaderKindVertex (ShaderErrorBadOutputType _) -> True; _ -> False },
    TestShaderCompileError { name = "ShaderErrorCouldNotLink", vert = "\\()()() -> ([0,0,0,0], 0)", frag = "\\()()() -> [0,0,0,0]", expect_error = \ e -> case e of ShaderError ShaderKindFragment (ShaderErrorCouldNotLink _ _) -> True; _ -> False },
    TestExprCompileError { name = "InterpreterErrorArrayIndexOutOfBounds", expr = "[0]!1", expect_error = \ e -> case e of InterpreterError _ (InterpreterErrorArrayIndexOutOfBounds 1) -> True; _ -> False },
    TestShaderCompileError { name = "InterpreterErrorDynamicTextureSelection", vert = "\\()()(v,c,b) -> (v, (b, c))", frag = "\\()([s,t] :: Tex 1D 2)(b, c) -> sample1D (if b then s else t) c", expect_error = \ e -> case e of InterpreterError _ (InterpreterErrorDynamicTextureSelection) -> True; _ -> False },
    TestShaderCompileError { name = "InterpreterErrorDynamicUnroll", vert = "\\(n)()() -> let id x = x in (unroll id n [0,0,0,0], ())", frag = "\\()()() -> [0,0,0,0]", expect_error = \ e -> case e of InterpreterError _ (InterpreterErrorDynamicUnroll) -> True; _ -> False },
    TestShaderCompileError { name = "InterpreterErrorDynamicIndex", vert = "\\(idx)()() -> ([0,0,0,0]!![idx,0,0,0], ())", frag = "\\()()() -> [0,0,0,0]", expect_error = \ e -> case e of InterpreterError _ (InterpreterErrorDynamicIndex) -> True; _ -> False },
    TestExprCompileError { name = "InterpreterErrorFunctionEquality", expr = "let id x = x in id == id", expect_error = \ e -> case e of InterpreterError _ (InterpreterErrorFunctionEquality) -> True; _ -> False }
    ] },
    
  TestGroup { group_title = "bug regressions", group_tests = [
    TestExprType { name = "should not trigger occurs check", expr = "\\(y :: (a, b)) -> [y, y]", expect_type = "(a, b) -> (a, b) 2" },
    TestExprType { name = "typing variable shadowing in lambda", expr = "(\\(x::Bool) -> \\(x::Real) -> x) True 1", expect_type = "Real" },
    TestExprValue { name = "interpreting variable shadowing in lambda", expr = "(\\(x::Bool) -> \\(x::Real) -> x) True 1", expect_value = "1" },
    TestExprType { name = "typing variable shadowing in let", expr = "let x = True in let x = 1 in x", expect_type = "Real" },
    TestExprValue { name = "interpreting variable shadowing in let", expr = "let x = True in let x = 1 in x", expect_value = "1" }
    ] }
  ]


-- Tests that an expression evaluates, or not, as expected.

runTest :: Test -> Result

runTest TestExprValue{ expr = esrc, expect_value = vsrc } =
  case evaluate library (ByteString.pack esrc) of
    Right (CommandResult _ _ v1 _) ->
      case evaluate library (ByteString.pack vsrc) of
        Right (CommandResult _ _ v2 _) -> if acceptablyEqualValues v1 v2 then ResultPass else ResultFail
        Left err -> ResultUnexpectedError err
    Left err -> ResultUnexpectedError err

runTest TestExprType{ expr = esrc, expect_type = tsrc } =
  case evaluate library (ByteString.pack esrc) of
    Right (CommandResult (_, _, vrefs) t1 _ _) ->
      case parseType vrefs (ByteString.pack tsrc) of
        Right (t2, _) -> if acceptablyEqualTypes t1 t2 then ResultPass else ResultFail
        Left err -> ResultUnexpectedError err
    Left err -> ResultUnexpectedError err

runTest TestExprCompileError{ expr = esrc, expect_error = checkerr } =
  case evaluate library (ByteString.pack esrc) of
    Right _ -> ResultFail
    Left err -> if checkerr err then ResultPass else ResultUnexpectedError err

runTest TestShaderCompileError{ vert = vertsrc, frag = fragsrc, expect_error = checkerr } =
  case compile (ByteString.pack vertsrc) (ByteString.pack fragsrc) of
    Right _ -> ResultFail
    Left err -> if checkerr err then ResultPass else ResultUnexpectedError err


-- Compares two values for equality up to a given relative error.

acceptableRelativeError :: Double
acceptableRelativeError = 0.001

acceptablyEqualValues :: Value -> Value -> Bool
acceptablyEqualValues (ValueUnit) (ValueUnit) = True
acceptablyEqualValues (ValueDFReal (DFRealLiteral _ d)) (ValueDFReal (DFRealLiteral _ d')) = abs ((d - d') / d) < acceptableRelativeError
acceptablyEqualValues (ValueDFBool (DFBoolLiteral _ b)) (ValueDFBool (DFBoolLiteral _ b')) = b == b'
acceptablyEqualValues (ValueTex tk i) (ValueTex tk' i') = tk == tk' && i == i'
acceptablyEqualValues (ValueArray vs) (ValueArray vs') = and $ zipWith acceptablyEqualValues vs vs'
acceptablyEqualValues (ValueTuple vs) (ValueTuple vs') = and $ zipWith acceptablyEqualValues vs vs'
acceptablyEqualValues (ValueFun _) (ValueFun _) = False
acceptablyEqualValues _ _ = False


-- Compares two types for equality up to variable references.

acceptablyEqualTypes :: Type -> Type -> Bool
acceptablyEqualTypes t1 t2 = canonicalType t1 == canonicalType t2

canonicalType :: Type -> Type
canonicalType t =
  case runTI (renameVarRefs nullSubst t) initFreshVarRefs of
    Right (s, _) -> applySubstType s t
    Left _ -> error $ "unexpected compile error in canonicalType!"

renameVarRefs :: Subst -> Type -> TI Subst
renameVarRefs sub (TypeUnit) = return sub
renameVarRefs sub (TypeReal) = return sub
renameVarRefs sub (TypeBool) = return sub
renameVarRefs sub (TypeTex _) = return sub
renameVarRefs sub (TypeArray t (DimFix _)) = renameVarRefs sub t
renameVarRefs sub (TypeArray t (DimVar dvref)) = do
  (tsub, dsub) <- renameVarRefs sub t
  case Map.lookup dvref dsub of
    Just _ -> return (tsub, dsub)
    Nothing -> do
      dv <- freshDimVar
      return (tsub, Map.insert dvref dv dsub)
renameVarRefs sub (TypeTuple ts) = foldM renameVarRefs sub ts
renameVarRefs sub (TypeFun t1 t2) = do
  sub' <- renameVarRefs sub t1
  renameVarRefs sub' t2
renameVarRefs (tsub, dsub) (TypeVar tvref) = do
  case Map.lookup tvref tsub of
    Just _ -> return (tsub, dsub)
    Nothing -> do
      tv <- freshTypeVar
      return (Map.insert tvref tv tsub, dsub)
