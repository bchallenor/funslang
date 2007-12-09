alex -g Lexer.x
happy -g -c Parser.y
ghc -Wall --make Main
pause