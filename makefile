run:
	dune exec -- ./bin/main.exe

auto:
	dune build --watch

clean:
	dune clean

build:
	dune build