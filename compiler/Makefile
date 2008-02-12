GHCFLAGS=-Wall

.PHONY: clean install

all: Parser.hs Lexer.hs lib standalone

lib: *.hs
	mkdir -p bin/notprof
	ghc $(GHCFLAGS) -odir bin/notprof -hidir bin/notprof -c --make LibFunslang
	ghc $(GHCFLAGS) -odir bin/notprof -c *.c -I../common/include -Iinclude
	ghc $(GHCFLAGS) -static -shared -o bin/funslang.dll funslang.def bin/notprof/*.o -package containers -package mtl -package fgl -package bytestring -package directory -package process -lopengl32 -Llib -ljpeg -L../common/bin -lglew32 -lglut32
	$(RM) bin/funslang.dll.a
	lib /machine:i386 /def:funslang.def /out:bin/funslang.lib

standalone: *.hs
	mkdir -p bin/prof
	ghc $(GHCFLAGS) -o bin/Main.exe -odir bin/prof -hidir bin/prof -prof -auto-all --make Main

Parser.hs: Parser.y
	happy -g -c -i Parser.y

Lexer.hs: Lexer.x
	alex -g Lexer.x

clean:
	-$(RM) bin/notprof/* bin/prof/* bin/*.exe bin/*.manifest bin/*.dll bin/*.a

install:
	mkdir -p ../common/lib
	cp -pf bin/funslang.dll ../common/bin/funslang.dll
	cp -pf bin/funslang.dll.a ../common/lib/funslang.lib
