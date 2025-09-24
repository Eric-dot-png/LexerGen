run:
	dune exec -- ./bin/main.exe

log:
	dune exec -- ./bin/main.exe > out.log

auto:
	dune build --watch

clean:
	dune clean

build:
	dune build