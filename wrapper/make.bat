ghc main.c ../common/funslang.c ../haskell/*.o -package containers -package mtl -package fgl -package bytestring -package directory -package process -Wall -Iinclude -I../common -I../haskell -L. -lglew32 -lglut32 -lopengl32