SOURCEDIR = src/
OUT_DIR = bin
HI_DIR = cache/hi_files
OBJ_DIR = cache/obj_files
HSFLAGS = -Wall -dynamic -hidir $(HI_DIR) -odir $(OBJ_DIR) -i$(SOURCEDIR) 
CFLAGS = -std=c11 -Wall -Wextra -O3

default: build

build:
	@mkdir -p $(OUT_DIR) $(HI_DIR) $(OBJ_DIR)
# 	gcc $(CFLAGS) -o $(OUT_DIR)/expr src/shunting_yard.c
	ghc $(HSFLAGS) -o $(OUT_DIR)/hs_expr src/Main.hs

.PHONY: clean
clean:
	rm -fr $(OUT_DIR) $(HI_DIR) $(OBJ_DIR)

.PHONY: test
test: build
	@test/run_tests
