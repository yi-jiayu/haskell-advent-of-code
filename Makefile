HS_SOURCES = $(wildcard src/day_*.hs)
HS_BINARIES = $(HS_SOURCES:src/day_%.hs=out/day_%.exe)
OUT = ./out

all: $(HS_BINARIES)

out/%.exe: src/%.hs
	ghc --make -outputdir $(OUT) -o $@ $<
