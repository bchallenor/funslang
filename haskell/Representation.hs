module Representation where

data Token
  = TOK_BOOL
  | TOK_INT
  | TOK_FLOAT
  --
  | TOK_TEXTURE1D
  | TOK_TEXTURE2D
  | TOK_TEXTURE3D
  | TOK_TEXTURECUBE
  --
  | TOK_LITERAL_BOOL !Bool
  | TOK_LITERAL_INT !Integer     -- arbitrary precision
  | TOK_LITERAL_FLOAT !Double    -- better than machine precision
  --
  | TOK_IDENTIFIER !String
  --
  | TOK_COMMA
  | TOK_VERTICAL_BAR
  | TOK_LBRACKET
  | TOK_RBRACKET
  | TOK_LPAREN
  | TOK_RPAREN
  --
  | TOK_OP_SUBSCRIPT
  | TOK_OP_SWIZZLE
  | TOK_OP_APPEND
  | TOK_OP_TRANSPOSE
  --
  | TOK_OP_MUL
  | TOK_OP_DIV
  | TOK_OP_LINEAR_MUL
  | TOK_OP_SCALE_MUL
  | TOK_OP_SCALE_DIV
  --
  | TOK_OP_ADD
  | TOK_OP_NEG_OP_SUB
  --
  | TOK_OP_LT
  | TOK_OP_GT
  | TOK_OP_LTE
  | TOK_OP_GTE
  --
  | TOK_OP_EQ
  | TOK_OP_NEQ
  --
  | TOK_OP_ID
  | TOK_OP_NID
  --
  | TOK_OP_AND
  | TOK_OP_XOR
  | TOK_OP_OR
  --
  | TOK_IF
  | TOK_THEN
  | TOK_ELSE
  | TOK_LET
  | TOK_EQUALS
  | TOK_IN
  | TOK_UPTO
  --
  | TOK_TYPESPECIFIER
  --
  | TOK_UNIFORM
  | TOK_TEXTURE
  --
  | TOK_FUN
  --
  | TOK_KERNEL
  | TOK_VERTEX
  | TOK_FRAGMENT
  
  deriving (Eq, Show)


data Type
  = UnitType
  | IntType
  | FloatType
  | BoolType
  | Texture1DType
  | Texture2DType
  | Texture3DType
  | TextureCubeType
  | TupleType [Type]
  | ArrayType Type Integer
  
  deriving (Show, Eq)


data Expr
  = UnitExpr
  | IntExpr !Integer
  | FloatExpr !Double
  | BoolExpr !Bool
  | VarExpr !String
  | NamedAppExpr !String !Expr
  | LibAppExpr !LibFunction !Expr
  | ArrayConsExpr ![Expr]
  | ArrayCompExpr !Expr !String !Expr !Expr
  | TupleExpr ![Expr]
  | IfExpr !Expr !Expr !Expr
  | LetExpr !Patt !Expr !Expr
  
  deriving (Show, Eq)


data Patt
  = VarPatt !String
  | ArrayConsPatt ![Patt]
  | TuplePatt ![Patt]
  
  deriving (Show, Eq)


type TypedIdent = (String, Type)


data AuxDecl
  = UniformDecl TypedIdent
  | TextureDecl TypedIdent
  | LetDecl !Patt !Expr
  | FunDecl !String ![TypedIdent] !Expr
  
  deriving (Show, Eq)


data KernelDecl
  = KernelDecl ![TypedIdent] !Expr
  
  deriving (Show, Eq)


data ProgramKind
  = VertProgram
  | FragProgram
  
  deriving (Show, Eq)


data Program
  = Program !ProgramKind ![AuxDecl] !KernelDecl
  
  deriving (Show, Eq)


data LibFunction
  = Lib_OP_SUBSCRIPT
  | Lib_OP_SWIZZLE
  | Lib_OP_APPEND
  | Lib_OP_TRANSPOSE
  | Lib_OP_NEG
  | Lib_OP_MUL
  | Lib_OP_DIV
  | Lib_OP_LINEAR_MUL
  | Lib_OP_SCALE_MUL
  | Lib_OP_SCALE_DIV
  | Lib_OP_ADD
  | Lib_OP_SUB
  | Lib_OP_LT
  | Lib_OP_LTE
  | Lib_OP_GT
  | Lib_OP_GTE
  | Lib_OP_EQ
  | Lib_OP_NEQ
  | Lib_OP_ID
  | Lib_OP_NID
  | Lib_OP_AND
  | Lib_OP_XOR
  | Lib_OP_OR
  | Lib_not
  | Lib_sum
  | Lib_product
  | Lib_any
  | Lib_all
  | Lib_sin
  | Lib_cos
  | Lib_tan
  | Lib_asin
  | Lib_acos
  | Lib_atan
  | Lib_pow
  | Lib_exp
  | Lib_exp2
  | Lib_log
  | Lib_log2
  | Lib_sqrt
  | Lib_rcpsqrt
  | Lib_rcp
  | Lib_abs
  | Lib_sign
  | Lib_floor
  | Lib_ceil
  | Lib_round
  | Lib_truncate
  | Lib_fract
  | Lib_mod
  | Lib_min
  | Lib_max
  | Lib_clamp
  | Lib_mix
  | Lib_step
  | Lib_smoothstep
  | Lib_length
  | Lib_distance
  | Lib_dot
  | Lib_cross
  | Lib_normalize
  | Lib_faceforward
  | Lib_reflect
  | Lib_refract
  | Lib_toInt
  | Lib_toFloat
  | Lib_sample
  | Lib_sampleProj
  | Lib_sampleLOD
  | Lib_sampleBias
  
  deriving (Show, Eq)
