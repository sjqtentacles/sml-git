# sml-git build
#
#   make            build the test binary with MLton (default)
#   make test       build + run tests under MLton
#   make test-poly  build + run tests under Poly/ML (via tools/polybuild)
#   make all-tests  run the suite under both compilers
#   make example    build + run the demo over the real fixture objects
#   make clean      remove build artifacts
#
# Layout B (dependent): our own sources live in src/; the single dependency
# sml-deflate (which bundles sml-codec and the string Zlib facade) is vendored
# under lib/ and loaded first, so the git sources see Zlib / Sha1 / Base16.

MLTON      ?= mlton
POLY       ?= poly
BIN        := bin
TEST_MLB   := test/sources.mlb
SRCS       := $(shell find lib src -name '*.sml' -o -name '*.sig' -o -name '*.mlb') \
              $(wildcard test/*.sml) $(TEST_MLB)

.PHONY: all test poly test-poly all-tests example clean

all: $(BIN)/test-mlton

$(BIN)/test-mlton: $(SRCS) | $(BIN)
	$(MLTON) -output $@ $(TEST_MLB)

test: $(BIN)/test-mlton
	$(BIN)/test-mlton

poly: $(BIN)/test-poly

# Poly/ML has no native .mlb support; tools/polybuild expands the flat .mlb,
# `use`s each source in order, and exports `main`.
$(BIN)/test-poly: $(SRCS) tools/polybuild | $(BIN)
	sh tools/polybuild -o $@ $(TEST_MLB)

test-poly: $(BIN)/test-poly
	$(BIN)/test-poly

all-tests: test test-poly

example: $(BIN)/demo
	./$(BIN)/demo

$(BIN)/demo: $(SRCS) examples/demo.sml examples/sources.mlb | $(BIN)
	$(MLTON) -output $@ examples/sources.mlb

$(BIN):
	mkdir -p $(BIN)

clean:
	rm -rf $(BIN)
