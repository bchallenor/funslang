{
{-# OPTIONS -w #-} -- suppress millions of Alex warnings
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
  "Real"                    { \s -> TOK_REAL }
  
  "Texture1D"               { \s -> TOK_TEXTURE1D }
  "Texture2D"               { \s -> TOK_TEXTURE2D }
  "Texture3D"               { \s -> TOK_TEXTURE3D }
  "TextureCube"             { \s -> TOK_TEXTURECUBE }
  
  "True"                    { \s -> TOK_LITERAL_BOOL True }
  "False"                   { \s -> TOK_LITERAL_BOOL False }
  $d+                       { \s -> TOK_LITERAL_INT (read s :: Integer) }
  $d+ "." $d+               { \s -> TOK_LITERAL_FLOAT (read s :: Double) }
  
  ","                       { \s -> TOK_COMMA }
  ".."                      { \s -> TOK_RANGE_DOTS }
  "["                       { \s -> TOK_LBRACKET }
  "]"                       { \s -> TOK_RBRACKET }
  "("                       { \s -> TOK_LPAREN }
  ")"                       { \s -> TOK_RPAREN }
  "_"                       { \s -> TOK_WILDCARD }
  
  "~"                       { \s -> TOK_OP_NOT }
  "!"                       { \s -> TOK_OP_SUBSCRIPT }
  "!!"                      { \s -> TOK_OP_SWIZZLE }
  "+"                       { \s -> TOK_OP_SCALAR_ADD }
  "-"                       { \s -> TOK_OP_SCALAR_NEG_OP_SCALAR_SUB }
  "*"                       { \s -> TOK_OP_SCALAR_MUL }
  "/"                       { \s -> TOK_OP_SCALAR_DIV }
  "++"                      { \s -> TOK_OP_VECTOR_ADD }
  "--"                      { \s -> TOK_OP_VECTOR_NEG_OP_VECTOR_SUB }
  "**"                      { \s -> TOK_OP_VECTOR_MUL }
  "//"                      { \s -> TOK_OP_VECTOR_DIV }
  "**."                     { \s -> TOK_OP_VECTOR_SCALAR_MUL }
  "//."                     { \s -> TOK_OP_VECTOR_SCALAR_DIV }
  "#"                       { \s -> TOK_OP_MATRIX_MATRIX_LINEAR_MUL }
  "#."                      { \s -> TOK_OP_MATRIX_VECTOR_LINEAR_MUL }
  ".#"                      { \s -> TOK_OP_VECTOR_MATRIX_LINEAR_MUL }
  "<"                       { \s -> TOK_OP_LT }
  ">"                       { \s -> TOK_OP_GT }
  "<="                      { \s -> TOK_OP_LTE }
  ">="                      { \s -> TOK_OP_GTE }
  "=="                      { \s -> TOK_OP_EQ }
  "/="                      { \s -> TOK_OP_NEQ }
  "&&"                      { \s -> TOK_OP_AND }
  "||"                      { \s -> TOK_OP_OR }
  "'"                       { \s -> TOK_OP_TRANSPOSE }
  
  "if"                      { \s -> TOK_IF }
  "then"                    { \s -> TOK_THEN }
  "else"                    { \s -> TOK_ELSE }
  "let"                     { \s -> TOK_LET }
  "="                       { \s -> TOK_EQUALS }
  "in"                      { \s -> TOK_IN }
  
  "::"                      { \s -> TOK_TYPESPECIFIER }
  "->"                      { \s -> TOK_RARROW }
  "\"                       { \s -> TOK_LAMBDA }
  "."                       { \s -> TOK_LAMBDA_DOT }
  
  "'" $a [$a $d _]*         { \s -> TOK_TYPE_VAR s }
  
  -- this goes last as it should not take precedence over the keywords
  $a [$a $d _]*             { \s -> TOK_IDENTIFIER s }


{
lexer :: String -> [Token]
lexer = alexScanTokens
}