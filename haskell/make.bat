del *.o
alex -g Lexer.x
happy -g -c -i Parser.y
ghc -Wall --make Main