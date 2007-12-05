{
module Lexer(Token(..), lexer) where
}


-- Use the basic wrapper.
%wrapper "basic"


-- Regex classes.
$d = [0-9]
$a = [a-zA-Z]
$s = [\ \t\n\f\v\r]


-- Token rules.
tokens :-
  
  $s+                       ; -- ignore whitespace
  "--" .*                   ; -- ignore comments
  
  "Bool"                    { \s -> TOK_BOOL }
  "Int"                     { \s -> TOK_INT }
  "Float"                   { \s -> TOK_FLOAT }
  
  "Texture1D"               { \s -> TOK_TEXTURE1D }
  "Texture2D"               { \s -> TOK_TEXTURE2D }
  "Texture3D"               { \s -> TOK_TEXTURE3D }
  "TextureCube"             { \s -> TOK_TEXTURECUBE }
  
  "true"                    { \s -> TOK_LITERAL_BOOL True }
  "false"                   { \s -> TOK_LITERAL_BOOL False }
  $d+                       { \s -> TOK_LITERAL_INT (read s :: Integer) }
  $d+ "." $d+               { \s -> TOK_LITERAL_FLOAT (read s :: Double) }
  
  ","                       { \s -> TOK_COMMA }
  "|"                       { \s -> TOK_VERTICAL_BAR }
  "["                       { \s -> TOK_LBRACKET }
  "]"                       { \s -> TOK_RBRACKET }
  "("                       { \s -> TOK_LPAREN }
  ")"                       { \s -> TOK_RPAREN }
  
  "!"                       { \s -> TOK_OP_SUBSCRIPT }
  "!!"                      { \s -> TOK_OP_SWIZZLE }
  "@"                       { \s -> TOK_OP_APPEND }
  "'"                       { \s -> TOK_OP_TRANSPOSE }
  
  "*"                       { \s -> TOK_OP_MUL }
  "/"                       { \s -> TOK_OP_DIV }
  "**"                      { \s -> TOK_OP_LINEAR_MUL }
  "*."                      { \s -> TOK_OP_SCALE_MUL }
  "/."                      { \s -> TOK_OP_SCALE_DIV }
  
  "+"                       { \s -> TOK_OP_ADD }
  "-"                       { \s -> TOK_OP_SUBNEG }
  
  "<"                       { \s -> TOK_OP_LT }
  ">"                       { \s -> TOK_OP_GT }
  "<="                      { \s -> TOK_OP_LTE }
  ">="                      { \s -> TOK_OP_GTE }
  
  "=="                      { \s -> TOK_OP_EQ }
  "/="                      { \s -> TOK_OP_NEQ }
  
  "==="                     { \s -> TOK_OP_ID }
  "/=="                     { \s -> TOK_OP_NID }
  
  "and"                     { \s -> TOK_OP_AND }
  "xor"                     { \s -> TOK_OP_XOR }
  "or"                      { \s -> TOK_OP_OR }
  
  "if"                      { \s -> TOK_IF }
  "then"                    { \s -> TOK_THEN }
  "else"                    { \s -> TOK_ELSE }
  "let"                     { \s -> TOK_LET }
  "="                       { \s -> TOK_EQUALS }
  "in"                      { \s -> TOK_IN }
  "upto"                    { \s -> TOK_UPTO }
  
  "::"                      { \s -> TOK_TYPESPECIFIER }
  
  "uniform"                 { \s -> TOK_UNIFORM }
  "texture"                 { \s -> TOK_TEXTURE }
  
  "fun"                     { \s -> TOK_FUN }
  
  "kernel"                  { \s -> TOK_KERNEL }
  "vshader"                 { \s -> TOK_VSHADER }
  "fshader"                 { \s -> TOK_FSHADER }
  
  -- this goes last as it should not take precendence over the keywords
  [$a _] [$a $d _]*         { \s -> TOK_IDENTIFIER s }


{
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
  | TOK_IDENTIFIER String
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

lexer :: String -> [Token]
lexer = alexScanTokens
}