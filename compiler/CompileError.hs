{-# OPTIONS -XFlexibleContexts #-} -- allow MonadError type annotation

-- This module defines error manipulation functions.
module CompileError where

import qualified Data.List as List
import Control.Monad.Error

import Representation
import Pretty


-- Stack trace gathering.

registerStackPoint :: (MonadError CompileError m) => Expr -> m a -> m a
registerStackPoint expr code =
  catchError code $
    \err -> case err of
      TypeError exprs specerr -> throwError $ TypeError (expr:exprs) specerr
      InterpreterError exprs specerr -> throwError $ InterpreterError (expr:exprs) specerr
      _ -> undefined


-- Error strings.

class StringableError e where
  getErrorString :: e -> String

instance StringableError CompileError where
  getErrorString (LexerError (line, col) specerr) = show line ++ ":" ++ show col ++ " " ++ getErrorString specerr
  getErrorString (ParserError (line, col) specerr) = show line ++ ":" ++ show col ++ " " ++ getErrorString specerr
  getErrorString (TypeError stack specerr) = getErrorString specerr ++ getStackString stack
  getErrorString (ShaderError sk specerr) = prettyShaderKind sk ++ " shader:\n" ++ getErrorString specerr
  getErrorString (InterpreterError stack specerr) = getErrorString specerr ++ getStackString stack
  getErrorString (OtherError msg) = msg

instance StringableError LexerError where
  getErrorString (LexerErrorNoLex c) = "lex error at <" ++ [c] ++ ">"
  getErrorString (LexerErrorIdentBeginsUpper s) = "bad identifier <" ++ s ++ ">: only types may begin uppercase"

instance StringableError ParserError where
  getErrorString (ParserErrorNoParse t) = "parse error at <" ++ show t ++ ">"
  getErrorString (ParserErrorBadFixedDim i) = "array dimension <" ++ show i ++ "> is invalid"

instance StringableError TypeError where
  getErrorString (TypeErrorOccursCheck t1 t2) = let [pt1, pt2] = prettyTypes [t1, t2] in "occurs check: " ++ pt1 ++ " = " ++ pt2
  getErrorString (TypeErrorCouldNotUnify t1 t2) = let [pt1, pt2] = prettyTypes [t1, t2] in "could not unify <" ++ pt1 ++ "> with <" ++ pt2 ++ ">"
  getErrorString (TypeErrorUnboundVariable ident) = "unbound variable <" ++ ident ++ ">"
  getErrorString (TypeErrorDuplicateIdentsInPattern p) = "names not unique in pattern: " ++ prettyPatt p

instance StringableError ShaderError where
  getErrorString (ShaderErrorBadShaderType t) = "bad shader type <" ++ prettyType t ++ ">"
  getErrorString (ShaderErrorBadUniformType t) = "shader uniform arguments cannot have type <" ++ prettyType t ++ ">"
  getErrorString (ShaderErrorBadTextureType t) = "shader texture arguments cannot have type <" ++ prettyType t ++ ">"
  getErrorString (ShaderErrorBadVaryingType t) = "shader varying arguments cannot have type <" ++ prettyType t ++ ">"
  getErrorString (ShaderErrorBadOutputType t) = "shader cannot output type <" ++ prettyType t ++ "> to next pipeline stage"
  getErrorString (ShaderErrorCouldNotLink t1 t2) = let [pt1, pt2] = prettyTypes [t1, t2] in "could not unify <" ++ pt1 ++ "> with <" ++ pt2 ++ "> to link shaders"

instance StringableError InterpreterError where
  getErrorString (InterpreterErrorArrayIndexOutOfBounds idx) = "array index <" ++ show idx ++ "> out of bounds"
  getErrorString (InterpreterErrorDynamicTextureSelection) = "cannot delay texture choice until runtime"
  getErrorString (InterpreterErrorDynamicUnroll) = "support for dynamic unroll count is not mandated"
  getErrorString (InterpreterErrorDynamicIndex) = "array index is not statically determinable"
  getErrorString (InterpreterErrorFunctionEquality) = "equality is not defined on functions"

getStackString :: [Expr] -> String
getStackString = concat . map (\e -> "\nin expression: " ++ prettyExpr e) . reverse
