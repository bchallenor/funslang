.PHONY: clean

all: Main.exe

Main.exe: *.hs
	ghc -Wall -prof -auto-all --make Main

Parser.hs: Parser.y
	happy -g -c -i Parser.y

Lexer.hs: Lexer.x
	alex -g Lexer.x

clean:
	-del *.o *.hi Main.exe.manifest Main.exe