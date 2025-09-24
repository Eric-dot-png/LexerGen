.PHONY: run 

IFILE ?= in.txt
OFILE ?= out.txt
DEBUG ?= true

run:
	@dune exec -- ./bin/main.exe -i $(IFILE) -o $(OFILE) $(if $(filter true,$(DEBUG)),-d)

log:
	@dune exec -- ./bin/main.exe > out.log

auto:
	@dune build --watch

clean:
	@dune clean

build:
	@dune build