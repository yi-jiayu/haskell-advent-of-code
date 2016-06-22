HS_SOURCES = $(wildcard src/*.hs)
HS_BINARIES = $(HS_SOURCES:src/%.hs=out/%.exe)

all: $(HS_BINARIES)

out/%.exe: src/%.hs
	ghc -o $@ -outputdir out $<
