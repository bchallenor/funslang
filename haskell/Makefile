GHCFLAGS=-Wall

.PHONY: clean

all: lib standalone

lib: *.hs
	ghc $(GHCFLAGS) -osuf notprof.o -hisuf notprof.hi -c --make LibFunslang
	ghc $(GHCFLAGS) -osuf notprof.o -hisuf notprof.hi -c LibFunslang_stub.c

standalone: *.hs
	ghc $(GHCFLAGS) -osuf prof.o -hisuf prof.hi -prof -auto-all --make Main

Parser.hs: Parser.y
	happy -g -c -i Parser.y

Lexer.hs: Lexer.x
	alex -g Lexer.x

clean:
	-del *.o *.hi *.exe *.manifest
