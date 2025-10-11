.PHONY: run 

IFILE ?= in.txt
OFILE ?= out.txt
DEBUG ?= true
OUT_DIR := output

run: $(OUT_DIR)
	@dune exec -- ./bin/main.exe -i $(IFILE) -o $(OFILE) $(if $(filter true,$(DEBUG)),-d)

log: $(OUT_DIR)
	@dune exec -- ./bin/main.exe > out.log

auto:
	@dune build --watch

clean:
	@dune clean

build:
	@dune build

$(OUT_DIR):
	@mkdir -p $(OUT_DIR)
