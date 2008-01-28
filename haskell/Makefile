GHCFLAGS=-Wall -prof -auto-all

.PHONY: clean

all: Main.exe libfunslang.a

libfunslang.a: *.hs
	ghc -c $(GHCFLAGS) --make LibFunslang
	ar -rcs $@ *.o

Main.exe: *.hs
	ghc $(GHCFLAGS) --make Main

Parser.hs: Parser.y
	happy -g -c -i Parser.y

Lexer.hs: Lexer.x
	alex -g Lexer.x

clean:
	-del *.o *.hi *.exe *.a *.manifest