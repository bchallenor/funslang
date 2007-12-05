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
  | TOK_OP_SUBNEG
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
  | TOK_VSHADER
  | TOK_FSHADER
  
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
