{
module Lexer(lexer) where
import Representation
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
  "--" .*                   ; -- ignore comments (upto newline)
  
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
  "-"                       { \s -> TOK_OP_NEG_OP_SUB }
  
  "<"                       { \s -> TOK_OP_LT }
  ">"                       { \s -> TOK_OP_GT }
  "<="                      { \s -> TOK_OP_LTE }
  ">="                      { \s -> TOK_OP_GTE }
  
  "=="                      { \s -> TOK_OP_EQ }
  "/="                      { \s -> TOK_OP_NEQ }
  
  "==="                     { \s -> TOK_OP_ID }
  "/=="                     { \s -> TOK_OP_NID }
  
  "and"                     { \s -> TOK_OP_AND }
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
  "vertex"                  { \s -> TOK_VERTEX }
  "fragment"                { \s -> TOK_FRAGMENT }
  
  -- this goes last as it should not take precendence over the keywords
  [$a _] [$a $d _]*         { \s -> TOK_IDENTIFIER s }


{
lexer :: String -> [Token]
lexer = alexScanTokens
}