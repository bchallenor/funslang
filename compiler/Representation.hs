module Representation where

import qualified Data.Map as Map
import qualified Data.IntMap as IntMap
import qualified Data.List as List
import qualified Data.Set as Set
import Control.Monad.State
import Control.Monad.Error
import Data.Graph


data ShaderKind
  = ShaderKindVertex
  | ShaderKindFragment
  
  deriving (Show, Eq)


data Token
  = TOK_REAL
  | TOK_BOOL
  --
  | TOK_TEX
  | TOK_TEXKIND1D
  | TOK_TEXKIND2D
  | TOK_TEXKIND3D
  | TOK_TEXKINDCUBE
  --
  | TOK_LITERAL_BOOL !Bool
  | TOK_LITERAL_INT !Integer     -- arbitrary precision
  | TOK_LITERAL_FLOAT !Double    -- better than machine precision
  --
  | TOK_IDENTIFIER !String
  --
  | TOK_COMMA
  | TOK_RANGE_DOTS
  | TOK_LBRACKET
  | TOK_RBRACKET
  | TOK_LPAREN
  | TOK_RPAREN
  | TOK_BACKTICK
  | TOK_WILDCARD
  --
  | TOK_OP_SUBSCRIPT
  | TOK_OP_SWIZZLE
  | TOK_OP_SCALAR_ADD
  | TOK_OP_SCALAR_NEG_OP_SCALAR_SUB
  | TOK_OP_SCALAR_MUL
  | TOK_OP_SCALAR_DIV
  | TOK_OP_VECTOR_ADD
  | TOK_OP_VECTOR_NEG_OP_VECTOR_SUB
  | TOK_OP_VECTOR_MUL
  | TOK_OP_VECTOR_DIV
  | TOK_OP_VECTOR_SCALAR_MUL
  | TOK_OP_VECTOR_SCALAR_DIV
  | TOK_OP_MATRIX_MATRIX_LINEAR_MUL
  | TOK_OP_MATRIX_VECTOR_LINEAR_MUL
  | TOK_OP_VECTOR_MATRIX_LINEAR_MUL
  | TOK_OP_LT
  | TOK_OP_GT
  | TOK_OP_LTE
  | TOK_OP_GTE
  | TOK_OP_EQ
  | TOK_OP_NEQ
  | TOK_OP_AND
  | TOK_OP_OR
  | TOK_OP_APPLY
  | TOK_OP_COMPOSE
  --
  | TOK_IF
  | TOK_THEN
  | TOK_ELSE
  | TOK_LET
  | TOK_EQUALS
  | TOK_IN
  --
  | TOK_TYPESPECIFIER
  | TOK_RARROW
  | TOK_LAMBDA
  --
  | TOK_EOF
  
  deriving (Eq, Show)


-- Rather than store identifiers for type/dim vars, we assign them numeric references.
-- These numbers are then converted back to a,b,c / m,n,o etc in the pretty print.
newtype TypeVarRef = TypeVarRef Int
  deriving (Eq, Ord)

instance Show TypeVarRef where
  show (TypeVarRef i) = "t" ++ show i

newtype DimVarRef = DimVarRef Int
  deriving (Eq, Ord)

instance Show DimVarRef where
  show (DimVarRef i) = "d" ++ show i

-- pools of fresh type/dim var refs, maps from identifiers to refs
type TypeEncodeContext = (([TypeVarRef], [DimVarRef]), (Map.Map String TypeVarRef, Map.Map String DimVarRef))
-- pools of fresh type/dim vars, maps from refs to identifiers
type TypeDecodeContext = (([String], [String]), (Map.Map TypeVarRef String, Map.Map DimVarRef String))

-- These variables are used in the internal representation of types.
-- We do not want to worry about alpha conversion, so they MUST NOT be reused.
-- They should only be used directly by the library initializer; after that you
-- need to ask the library which fresh variables remain.
initFreshVarRefs :: ([TypeVarRef], [DimVarRef])
initFreshVarRefs = (map TypeVarRef [0..], map DimVarRef [0..])

-- These variables are only used in converting an internal type to an external type,
-- so they can be reused as often as you like.
-- Thanks to oerjan on #haskell for this trick.
initFreshVars :: ([String], [String])
initFreshVars = ([1..] >>= flip replicateM ['a'..'g'], [1..] >>= flip replicateM ['m'..'z'])


-- texture kinds
data TexKind
  = TexKind1D
  | TexKind2D
  | TexKind3D
  | TexKindCube
  
  deriving (Eq)

instance Show TexKind where
  show TexKind1D = "1D"
  show TexKind2D = "2D"
  show TexKind3D = "3D"
  show TexKindCube = "Cube"


-- as dims appear externally (in source or pretty-printed form)
data ExDim
  = ExDimFix !Integer
  | ExDimVar !String
  
  deriving (Show, Eq)


-- as types appear externally (in source or pretty-printed form)
data ExType
  = ExTypeUnit
  | ExTypeReal
  | ExTypeBool
  | ExTypeTex !TexKind
  | ExTypeArray !ExType !ExDim
  | ExTypeTuple ![ExType]
  | ExTypeFun !ExType !ExType
  | ExTypeVar !String
  
  deriving (Show, Eq)


-- as dims are represented internally
data Dim
  = DimFix !Integer
  | DimVar !DimVarRef
  
  deriving (Show, Eq)


-- as types are represented internally
data Type
  = TypeUnit
  | TypeReal
  | TypeBool
  | TypeTex !TexKind
  | TypeArray !Type !Dim
  | TypeTuple ![Type]
  | TypeFun !Type !Type
  | TypeVar !TypeVarRef
  
  deriving (Show, Eq)



-- VarRefs will represent sets of free or bound type and dim vars.
type VarRefs = (Set.Set TypeVarRef, Set.Set DimVarRef)

emptyVarRefs :: VarRefs
emptyVarRefs = (Set.empty, Set.empty)

unionVarRefs :: VarRefs -> VarRefs -> VarRefs
unionVarRefs (l1, r1) (l2, r2) = (Set.union l1 l2, Set.union r1 r2)

differenceVarRefs :: VarRefs -> VarRefs -> VarRefs
differenceVarRefs (l1, r1) (l2, r2) = (Set.difference l1 l2, Set.difference r1 r2)

-- A type scheme generalizes a type over the bound type vars and dim vars.
data Scheme = Scheme !VarRefs !Type deriving (Show, Eq)

-- A type environment maps identifiers to type schemes.
type SchemeEnv = Map.Map String Scheme

-- The environment of values as being interpreted.
type ValueEnv = Map.Map String (InterpretM Value)

-- A tuple of:
-- - a map from library identifiers to type schemes;
-- - a map from library identifiers to values;
-- - the subsequent list of fresh variable references, given that some were used
--   in constructing the type schemes of the library functions.
type Library = (SchemeEnv, ValueEnv, ([TypeVarRef], [DimVarRef]))


-- Encodes an external type (given pools of fresh var refs).
typeFromExType :: ([TypeVarRef], [DimVarRef]) -> ExType -> (Type, ([TypeVarRef], [DimVarRef]))
typeFromExType fresh_vrefs xt =
  let (t, (fresh_vrefs', _)) = runState (typeFromExType' xt) (fresh_vrefs, (Map.empty, Map.empty)) in
    (t, fresh_vrefs')

-- Encodes many external types (in the same context).
typesFromExTypes :: ([TypeVarRef], [DimVarRef]) -> [ExType] -> ([Type], ([TypeVarRef], [DimVarRef]))
typesFromExTypes fresh_vrefs xts =
  let (t, (fresh_vrefs', _)) = runState (mapM typeFromExType' xts) (fresh_vrefs, (Map.empty, Map.empty)) in
    (t, fresh_vrefs')

typeFromExType' :: ExType -> State TypeEncodeContext Type
typeFromExType' (ExTypeUnit) = return TypeUnit
typeFromExType' (ExTypeReal) = return TypeReal
typeFromExType' (ExTypeBool) = return TypeBool
typeFromExType' (ExTypeTex tk) = return $ TypeTex tk
typeFromExType' (ExTypeTuple xts) = do
  ts <- mapM typeFromExType' xts
  return $ TypeTuple ts
typeFromExType' (ExTypeArray xt (ExDimFix i)) = do
  t <- typeFromExType' xt
  return $ TypeArray t (DimFix i)
typeFromExType' (ExTypeFun xt1 xt2) = do
  t1 <- typeFromExType' xt1
  t2 <- typeFromExType' xt2
  return $ TypeFun t1 t2
typeFromExType' a@(ExTypeVar tv) = do
  ((fresh_tvref:fresh_tvrefs, fresh_dvrefs), (tv_to_tvref, dv_to_dvref)) <- get
  case Map.lookup tv tv_to_tvref of
    Just tvref -> return $ TypeVar tvref
    Nothing -> do
      put ((fresh_tvrefs, fresh_dvrefs), (Map.insert tv fresh_tvref tv_to_tvref, dv_to_dvref))
      typeFromExType' a
typeFromExType' a@(ExTypeArray xt (ExDimVar dv)) = do
  t <- typeFromExType' xt
  ((fresh_tvrefs, fresh_dvref:fresh_dvrefs), (tv_to_tvref, dv_to_dvref)) <- get
  case Map.lookup dv dv_to_dvref of
    Just dvref -> return $ TypeArray t (DimVar dvref)
    Nothing -> do
      put ((fresh_tvrefs, fresh_dvrefs), (tv_to_tvref, Map.insert dv fresh_dvref dv_to_dvref))
      typeFromExType' a


-- Decodes an internal type.
exTypeFromType :: Type -> ExType
exTypeFromType t = evalState (exTypeFromType' t) (initFreshVars, (Map.empty, Map.empty))

-- Decodes many internal types (in the same context).
exTypesFromTypes :: [Type] -> [ExType]
exTypesFromTypes ts = evalState (mapM exTypeFromType' ts) (initFreshVars, (Map.empty, Map.empty))

exTypeFromType' :: Type -> State TypeDecodeContext ExType
exTypeFromType' (TypeUnit) = return ExTypeUnit
exTypeFromType' (TypeReal) = return ExTypeReal
exTypeFromType' (TypeBool) = return ExTypeBool
exTypeFromType' (TypeTex tk) = return $ ExTypeTex tk
exTypeFromType' (TypeTuple ts) = do
  xts <- mapM exTypeFromType' ts
  return $ ExTypeTuple xts
exTypeFromType' (TypeArray t (DimFix i)) = do
  xt <- exTypeFromType' t
  return $ ExTypeArray xt (ExDimFix i)
exTypeFromType' (TypeFun t1 t2) = do
  xt1 <- exTypeFromType' t1
  xt2 <- exTypeFromType' t2
  return $ ExTypeFun xt1 xt2
exTypeFromType' a@(TypeVar tvref) = do
  ((fresh_tv:fresh_tvs, fresh_dvs), (tvref_to_tv, dvref_to_dv)) <- get
  case Map.lookup tvref tvref_to_tv of
    Just tv -> return $ ExTypeVar tv
    Nothing -> do
      put ((fresh_tvs, fresh_dvs), (Map.insert tvref fresh_tv tvref_to_tv, dvref_to_dv))
      exTypeFromType' a
exTypeFromType' a@(TypeArray t (DimVar dvref)) = do
  xt <- exTypeFromType' t
  ((fresh_tvs, fresh_dv:fresh_dvs), (tvref_to_tv, dvref_to_dv)) <- get
  case Map.lookup dvref dvref_to_dv of
    Just dv -> return $ ExTypeArray xt (ExDimVar dv)
    Nothing -> do
      put ((fresh_tvs, fresh_dvs), (tvref_to_tv, Map.insert dvref fresh_dv dvref_to_dv))
      exTypeFromType' a


data Expr
  = ExprUnitLiteral
  | ExprRealLiteral !Double
  | ExprBoolLiteral !Bool
  | ExprVar !String
  | ExprApp !Expr !Expr
  | ExprArray ![Expr]
  | ExprTuple ![Expr]
  | ExprIf !Expr !Expr !Expr
  | ExprLet !Patt !Expr !Expr -- pattern, bound expression, body expression
  | ExprLambda !Patt !Expr -- pattern, body expression
  
  deriving (Show, Eq)


data Patt
  = PattWild !(Maybe Type)
  | PattUnit !(Maybe Type)
  | PattVar !String !(Maybe Type)
  | PattArray ![Patt] !(Maybe Type)
  | PattTuple ![Patt] !(Maybe Type)
  
  deriving (Show, Eq)


data Operator
  = OpScalarNeg
  | OpVectorNeg
  --
  | OpSubscript
  | OpSwizzle
  | OpScalarAdd
  | OpScalarSub
  | OpScalarMul
  | OpScalarDiv
  | OpVectorAdd
  | OpVectorSub
  | OpVectorMul
  | OpVectorDiv
  | OpVectorScalarMul
  | OpVectorScalarDiv
  | OpMatrixMatrixLinearMul
  | OpMatrixVectorLinearMul
  | OpVectorMatrixLinearMul
  | OpLessThan
  | OpGreaterThan
  | OpLessThanEqual
  | OpGreaterThanEqual
  | OpEqual
  | OpNotEqual
  | OpAnd
  | OpOr
  | OpApply
  | OpCompose
  
  deriving Eq


instance Show Operator where

  show OpScalarNeg = "negate"
  show OpVectorNeg = "negates"
  --
  show OpSubscript = "!"
  show OpSwizzle = "!!"
  show OpScalarAdd = "+"
  show OpScalarSub = "-"
  show OpScalarMul = "*"
  show OpScalarDiv = "/"
  show OpVectorAdd = "++"
  show OpVectorSub = "--"
  show OpVectorMul = "**"
  show OpVectorDiv = "//"
  show OpVectorScalarMul = "**."
  show OpVectorScalarDiv = "//."
  show OpMatrixMatrixLinearMul = "#"
  show OpMatrixVectorLinearMul = "#."
  show OpVectorMatrixLinearMul = ".#"
  show OpLessThan = "<"
  show OpGreaterThan = ">"
  show OpLessThanEqual = "<="
  show OpGreaterThanEqual = ">="
  show OpEqual = "=="
  show OpNotEqual = "/="
  show OpAnd = "&&"
  show OpOr = "||"
  show OpApply = "$"
  show OpCompose = "."


-- Commands are used only in the interactive debugger.
data Command
  = CommandExpr !Expr
  | CommandLet !Patt !Expr -- pattern, bound expression

-- library after binding, type of pattern, value bound, interpreter state after binding
data CommandResult
  = CommandResult !Library !Type !Value !InterpretState


-- Dataflow graph.

data DF
  = DFReal !DFReal
  | DFBool !DFBool
  | DFSample !DFSample
  
  deriving (Show, Eq)


type DFID = Int -- unique number for node


data DFReal
  = DFRealLiteral !DFID !Double
  | DFRealVarying !DFID !Int -- the global scalar index among the varyings
  | DFRealUniform !DFID !Int -- the global scalar index among the uniforms
  --
  | DFRealCond !DFID !DFBool !DFReal !DFReal
  --
  | DFRealAdd !DFID !DFReal !DFReal
  | DFRealSub !DFID !DFReal !DFReal
  | DFRealMul !DFID !DFReal !DFReal
  | DFRealDiv !DFID !DFReal !DFReal
  | DFRealNeg !DFID !DFReal
  | DFRealRcp !DFID !DFReal
  | DFRealRsq !DFID !DFReal
  | DFRealAbs !DFID !DFReal
  | DFRealMin !DFID !DFReal !DFReal
  | DFRealMax !DFID !DFReal !DFReal
  | DFRealFloor !DFID !DFReal
  | DFRealCeiling !DFID !DFReal
  | DFRealRound !DFID !DFReal
  | DFRealTruncate !DFID !DFReal
  | DFRealFract !DFID !DFReal
  | DFRealExp !DFID !DFReal
  | DFRealExp2 !DFID !DFReal
  | DFRealLog !DFID !DFReal
  | DFRealLog2 !DFID !DFReal
  | DFRealPow !DFID !DFReal !DFReal
  | DFRealSin !DFID !DFReal
  | DFRealCos !DFID !DFReal
  | DFRealTan !DFID !DFReal
  | DFRealASin !DFID !DFReal
  | DFRealACos !DFID !DFReal
  | DFRealATan !DFID !DFReal
  --
  | DFRealGetTexR !DFID !DFSample
  | DFRealGetTexG !DFID !DFSample
  | DFRealGetTexB !DFID !DFSample
  | DFRealGetTexA !DFID !DFSample

  deriving (Show, Eq)


data DFBool
  = DFBoolLiteral !DFID !Bool
  | DFBoolVarying !DFID !Int -- the global scalar index among the varyings
  | DFBoolUniform !DFID !Int -- the global scalar index among the uniforms
  --
  | DFBoolCond !DFID !DFBool !DFBool !DFBool
  --
  | DFBoolLessThan !DFID !DFReal !DFReal
  | DFBoolLessThanEqual !DFID !DFReal !DFReal
  | DFBoolGreaterThan !DFID !DFReal !DFReal
  | DFBoolGreaterThanEqual !DFID !DFReal !DFReal
  --
  | DFBoolEqualReal !DFID !DFReal !DFReal
  | DFBoolNotEqualReal !DFID !DFReal !DFReal
  | DFBoolEqualBool !DFID !DFBool !DFBool
  | DFBoolNotEqualBool !DFID !DFBool !DFBool
  --
  | DFBoolAnd !DFID !DFBool !DFBool
  | DFBoolOr !DFID !DFBool !DFBool
  | DFBoolNot !DFID !DFBool

  deriving (Show, Eq)


data DFSample -- these are internal to a texture sampling gadget
  = DFSampleTex !DFID !TexKind !Int ![DFReal] -- texture kind, texture image unit, coords

  deriving (Show, Eq)


-- Dependency graph.
-- Each edge (a,b) in the Graph means that b depends on a, or equivalently
-- that a must be calculated before b.
type DFGraph = (
  Graph, -- graph representation
  [DF], -- result nodes
  IntMap.IntMap DF -- mapping from vertices to nodes, for understanding the topological sort
  )


-- The interpreter monad holds fresh numbers,
-- and can return errors (use throwError).
type InterpretM a = ErrorT CompileError (State InterpretState) a


data InterpretState
  = InterpretState
  {
    num_uniforms :: !Int,
    num_textures :: !Int,
    num_varyings :: !Int,
    textures :: ![(TexKind, Int)], -- texture kind, texture image unit
    num_generic_outputs :: !Int, -- not including position, color etc
    num_nodes :: !Int -- node count so that new ones get unique IDs
  }
  
  deriving (Show, Eq)


initInterpretState :: InterpretState
initInterpretState = InterpretState{num_uniforms = 0, num_textures = 0, num_varyings = 0, textures = [], num_generic_outputs = 0, num_nodes = 0}


runInterpretM :: InterpretM a -> InterpretState -> Either CompileError (a, InterpretState)
runInterpretM vi i = do
  let (a, i') = runState (runErrorT vi) i
  r <- a
  return (r, i')

freshUniform :: InterpretM Int
freshUniform = do
  s <- get
  let i = num_uniforms s
  put s{num_uniforms = i+1}
  return i

freshTexture :: TexKind -> InterpretM Int
freshTexture tk = do
  s <- get
  let i = num_textures s
  let ts = textures s
  put s{num_textures = i+1, textures = (tk, i) : ts}
  return i

freshVarying :: InterpretM Int
freshVarying = do
  s <- get
  let i = num_varyings s
  put s{num_varyings = i+1}
  return i

freshGenericOutput :: InterpretM ()
freshGenericOutput = do
  s <- get
  let i = num_generic_outputs s
  put s{num_generic_outputs = i+1}
  return ()

freshNode :: InterpretM Int
freshNode = do
  s <- get
  let i = num_nodes s
  put s{num_nodes = i+1}
  return i


data Value -- can't derive Show or Eq due to those pesky closures
  = ValueUnit
  | ValueDFReal !DFReal
  | ValueDFBool !DFBool
  | ValueTex !TexKind !Int  -- texture kind, texture image unit
  | ValueArray ![Value]
  | ValueTuple ![Value]
  | ValueFun !(Value -> InterpretM Value) -- might raise exception

unValueDFReal :: Value -> DFReal
unValueDFReal (ValueDFReal df) = df
unValueDFReal _ = undefined

unValueDFBool :: Value -> DFBool
unValueDFBool (ValueDFBool df) = df
unValueDFBool _ = undefined

instance Show Value where

  show (ValueUnit) = "()"
  show (ValueDFReal (DFRealLiteral _ r)) = show r
  show (ValueDFReal df) = show df
  show (ValueDFBool (DFBoolLiteral _ b)) = show b
  show (ValueDFBool df) = show df
  show (ValueTex tk i) = "texture[" ++ show i ++ ", " ++ show tk ++ "]"
  show (ValueArray vs) = "[" ++ (concat $ List.intersperse ", " $ map show vs) ++ "]"
  show (ValueTuple vs) = "(" ++ (concat $ List.intersperse ", " $ map show vs) ++ ")"
  show (ValueFun _) = "<function>"

instance Eq Value where

  (ValueUnit) == (ValueUnit) = True
  (ValueDFReal df) == (ValueDFReal df') = df == df'
  (ValueDFBool df) == (ValueDFBool df') = df == df'
  (ValueTex tk i) == (ValueTex tk' i') = tk == tk' && i == i'
  (ValueArray vs) == (ValueArray vs') = vs == vs'
  (ValueTuple vs) == (ValueTuple vs') = vs == vs'
  (ValueFun _) == (ValueFun _) = False
  _ == _ = False


-- Following are ALL errors that can be triggered by user input.

instance Error CompileError where
  strMsg = OtherError

data CompileError
  = LexerError !(Int, Int) !LexerError -- line, col
  | ParserError !(Int, Int) !ParserError -- line, col
  | TypeError ![Expr] !TypeError -- stack trace
  | ShaderError !ShaderKind !ShaderError
  | InterpreterError ![Expr] !InterpreterError -- stack trace
  
  -- OtherError is required for Control.Monad.Error compatibility only!
  -- If you need new errors, please define them in a type-safe way.
  | OtherError !String
  
  deriving Show

data LexerError
  = LexerErrorNoLex !Char
  | LexerErrorIdentBeginsUpper !String
  
  deriving Show

data ParserError
  = ParserErrorNoParse !Token
  | ParserErrorBadFixedDim !Integer
  
  deriving Show

data TypeError
  = TypeErrorOccursCheck !Type !Type
  | TypeErrorCouldNotUnify !Type !Type
  | TypeErrorUnboundVariable !String
  | TypeErrorDuplicateIdentsInPattern !Patt
  
  deriving Show

data ShaderError
  = ShaderErrorBadShaderType !Type
  | ShaderErrorBadUniformType !Type
  | ShaderErrorBadTextureType !Type
  | ShaderErrorBadVaryingType !Type
  | ShaderErrorBadOutputType !Type
  | ShaderErrorCouldNotLink !Type !Type
  
  deriving Show

data InterpreterError
  = InterpreterErrorArrayIndexOutOfBounds !Int
  | InterpreterErrorDynamicTextureSelection
  | InterpreterErrorDynamicUnroll
  | InterpreterErrorDynamicIndex
  | InterpreterErrorFunctionEquality
  
  deriving Show
