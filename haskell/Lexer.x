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
  "Int"                     { \s -> TOK_INT }
  "Float"                   { \s -> TOK_FLOAT }
  
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
  
  "!"                       { \s -> TOK_OP_SUBSCRIPT }
  "!!"                      { \s -> TOK_OP_SWIZZLE }
  "@"                       { \s -> TOK_OP_APPEND }
  "'"                       { \s -> TOK_OP_TRANSPOSE }
  "~"                       { \s -> TOK_OP_NOT }
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
  "&&"                      { \s -> TOK_OP_AND }
  "||"                      { \s -> TOK_OP_OR }
  
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
  
  -- this goes last as it should not take precendence over the keywords
  $a [$a $d]*               { \s -> TOK_IDENTIFIER s }


{
lexer :: String -> [Token]
lexer = alexScanTokens
}