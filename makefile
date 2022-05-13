SHELL := /bin/sh
OUT_DIR := bin
OUT_EXE := $(OUT_DIR)/hscalc
CACHE := .cache
HSFLAGS := -dynamic -j -O1 -fmax-errors=1
GHCWARNS := -Wall -Wextra -Wmissing-exported-signatures -Widentities \
            -Wpartial-fields -Wredundant-constraints
GHCEXTS := -XOverloadedStrings -XLambdaCase -XStrict \
           -XScopedTypeVariables -XImportQualifiedPost
GHCFLAGS := -outputdir $(CACHE) $(GHCWARNS) $(GHCEXTS) $(HSFLAGS)

.PHONY: default
default: $(OUT_EXE)

$(OUT_EXE): src/*.hs | $(OUT_DIR) $(CACHE)
	@echo ghc $(HSFLAGS) -o $@ src/*.hs
	@ghc $(GHCFLAGS) -o $@ src/*.hs

$(OUT_DIR) $(CACHE):
	mkdir -p $@

.PHONY: clean
clean:
	rm -fr $(OUT_DIR) $(CACHE)

.PHONY: test
test: $(OUT_EXE)
	@test/run_tests $(OUT_EXE)
	@echo "all tests passing! :^D"
