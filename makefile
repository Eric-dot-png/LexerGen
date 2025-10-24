.PHONY: run 

OUT_DIR := output

IFILE ?= in.txt
OFILE ?= $(OUT_DIR)/out.cpp
DEBUG ?= true

run: $(OUT_DIR)
	@dune exec -- ./bin/main.exe -i $(IFILE) -o $(OFILE) $(if $(filter true,$(DEBUG)),-d)

log: $(OUT_DIR)
	@dune exec -- ./bin/main.exe -i $(IFILE) -o $(OFILE) $(if $(filter true,$(DEBUG)),-d) > $(OUT_DIR)/out.log

auto:
	@dune build --watch

clean:
	@dune clean

build:
	@dune build

$(OUT_DIR):
	@mkdir -p $(OUT_DIR)
