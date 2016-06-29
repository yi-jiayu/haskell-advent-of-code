HS_SOURCES = $(wildcard src/day_*.hs)
HS_BINARIES = $(HS_SOURCES:src/day_%.hs=out/day_%.exe)
OUT = ./out

all: $(HS_BINARIES)

out/%.exe: src/%.hs Welcome.hs
	ghc --make -O2 -outputdir intermediate -o $@ $<
	@rm -f intermediate/Main.*

.PHONY clean:
	rm -f $(OUT)/*
	rf -f intermediate/*
