SHELL := /bin/sh
OUT_DIR := bin
OUT_EXE := $(OUT_DIR)/hscalc
HI_DIR := cache/hi_files
OBJ_DIR := cache/obj_files
HSFLAGS := -Wall -dynamic -j -XStrict
GHCFLAGS := -hidir $(HI_DIR) -odir $(OBJ_DIR)

.PHONY: default
default: $(OUT_EXE)

$(OUT_EXE): src/*.hs | $(OUT_DIR) $(HI_DIR) $(OBJ_DIR)
	ghc $(HSFLAGS) $(GHCFLAGS) -o $@ src/*.hs

$(OUT_DIR) $(HI_DIR) $(OBJ_DIR):
	mkdir -p $@

.PHONY: clean
clean:
	rm -fr $(OUT_DIR) $(HI_DIR) $(OBJ_DIR)

.PHONY: test
test: build
	@test/run_tests
	@echo "all tests passing! :^D"
